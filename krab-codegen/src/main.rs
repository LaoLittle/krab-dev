use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::{IntPredicate, OptimizationLevel};
use std::collections::HashMap;

use inkwell::basic_block::BasicBlock;
use inkwell::data_layout::DataLayout;
use inkwell::passes::{PassBuilderOptions, PassManager};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::types::{
    AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType,
    PointerType,
};
use inkwell::values::{
    AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue,
    PointerValue,
};
use kotlin_ast::expr::{BinaryOp, ExprStmt};
use kotlin_parse::Parser;
use kotlin_span::symbol::Symbol;
use kotlin_span::{
    with_global_session_init, BOOLEAN, FLOAT32, FLOAT64, INT16, INT32, INT64, INT8, UINT16, UINT32,
    UINT64, UINT8, UNIT,
};
use krab_lowering::{LoweringContext, TirCtx};
use krab_tir::expr::{Expr, ExprKind, Literal};
use krab_tir::stmt::{Decl, Stmt};
use krab_tir::ty::Type;
use std::error::Error;
use std::ops::Deref;
use std::path::Path;

pub struct CodegenContext<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    scopes: Vec<HashMap<Symbol, (PointerValue<'ctx>, Type)>>,
    fns: Vec<HashMap<Symbol, FunctionValue<'ctx>>>,
    current_fn: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodegenContext<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>, builder: Builder<'ctx>) -> Self {
        Self {
            context,
            module,
            builder,
            scopes: Vec::new(),
            fns: Vec::new(),
            current_fn: None,
        }
    }

    pub fn visit_stmt(&mut self, stmt: &Stmt) -> Option<BasicValueEnum<'ctx>> {
        match stmt {
            Stmt::Decl(decl) => {
                self.visit_decl(decl);
                None
            }
            Stmt::Expr(expr) => self.visit_expr(expr),
        }
    }

    pub fn visit_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Fun(name, ret_ty, args_ty, body) => {
                self.enter_scope();

                let mut args: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(args_ty.len());

                for (_, arg) in *args_ty {
                    args.push(self.ty_to_llvm(arg).unwrap().into());
                }

                let fn_ty = self
                    .ty_to_llvm(ret_ty)
                    .map(|ty| ty.fn_type(&args, false))
                    .unwrap_or_else(|| self.context.void_type().fn_type(&args, false));

                let fn_val = self
                    .module
                    .add_function(&name.symbol().as_str(), fn_ty, None);

                self.current_fn = Some(fn_val);

                self.current_fns().insert(name.symbol(), fn_val);

                let bb = self.context.append_basic_block(fn_val, "");
                self.builder.position_at_end(bb);

                let mut i = 0;
                for (name, ty) in *args_ty {
                    let ptr = self.build_alloca(self.ty_to_llvm(ty).unwrap(), "").unwrap();
                    let val = fn_val.get_nth_param(i).unwrap();
                    self.build_store(ptr, val).unwrap();
                    i += 1;
                    self.current_scope()
                        .insert(name.symbol(), (ptr, ty.clone()));
                }

                let mut returned = false;

                for stmt in body.stmts {
                    if let Stmt::Expr(expr) = stmt {
                        if matches!(expr.kind, ExprKind::Return(_)) {
                            returned = true;
                        }
                    }

                    self.visit_stmt(stmt);

                    if returned {
                        break;
                    } // dead code.
                }

                if !returned && ret_ty.is_unit() {
                    self.builder.build_return(None).unwrap();
                }

                self.current_fn = None;

                self.exit_scope();

                self.current_fns().insert(name.symbol(), fn_val);
            }
            Decl::Var(name, init) => {
                let Some(init) = init else {
                    return;
                };

                let ty = self.ty_to_llvm(&init.ty).unwrap();

                let alloca = self.builder.build_alloca(ty, "").unwrap();
                self.current_scope()
                    .insert(name.symbol(), (alloca, init.ty.clone()));

                let expr = self.visit_expr(init).unwrap();

                self.builder.build_store(alloca, expr).unwrap();
            }
        }
    }

    pub fn visit_expr(&mut self, expr: &Expr) -> Option<BasicValueEnum<'ctx>> {
        match expr.kind {
            ExprKind::Variable(id) => {
                let (ptr, ty) = self.current_scope().get(&id.symbol()).unwrap();
                let (ptr, ty) = (*ptr, ty.clone());

                let llty = self.ty_to_llvm(&ty).unwrap();
                let var = self.builder.build_load(llty, ptr, "").unwrap();

                Some(var)
            }
            ExprKind::Call(callable, args) => {
                let mut llargs: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

                for arg in args {
                    llargs.push(self.visit_expr(arg).unwrap().into());
                }

                match callable.kind {
                    ExprKind::Variable(name) => {
                        let f = self.find_fn(&name.symbol()).unwrap();

                        self.builder
                            .build_direct_call(f, &llargs, "")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                    }
                    _ => {
                        let val = self.visit_expr(callable).unwrap();

                        todo!()
                    }
                }
            }
            ExprKind::Lit(lit) => Some(self.lit_to_llvm(&lit, &expr.ty)),
            ExprKind::Binary(binop, lhs, rhs) => {
                let bool_ty = self.context.bool_type();
                match binop {
                    BinaryOp::LAnd => {
                        // alloca t = false
                        // a && b ->
                        // if a goto L1 else L3
                        // L1:
                        // if b goto L2 else L3
                        // L2:
                        // t = true
                        // goto L3
                        // L3:
                        // *code*
                        let t = self.build_alloca(self.context.bool_type(), "").unwrap();
                        self.build_store(t, self.const_bool(false)).unwrap();

                        let l1 = self.append_bb();
                        let l2 = self.append_bb();
                        let l3 = self.append_bb();
                        // L0:
                        let lhs = self.visit_expr(lhs).unwrap().into_int_value();
                        self.build_conditional_branch(lhs, l1, l3).unwrap();
                        self.position_at_end(l1);
                        // L1:
                        let rhs = self.visit_expr(rhs).unwrap().into_int_value();
                        self.build_conditional_branch(rhs, l2, l3).unwrap();
                        self.position_at_end(l2);
                        // L2:
                        self.build_store(t, self.const_bool(true)).unwrap();
                        self.build_unconditional_branch(l3).unwrap();
                        // L3:
                        self.position_at_end(l3);

                        println!("What");

                        return Some(self.build_load(bool_ty, t, "").unwrap());
                    }
                    BinaryOp::LOr => {
                        // alloca t = false
                        // a || b ->
                        // if a goto L2 else L1
                        // L1:
                        // if b goto L2 else L3
                        // L2:
                        // t = true
                        // goto L3
                        // L3:
                        // *code*
                        let t = self.build_alloca(self.context.bool_type(), "").unwrap();
                        self.build_store(t, self.const_bool(false)).unwrap();

                        let l1 = self.append_bb();
                        let l2 = self.append_bb();
                        let l3 = self.append_bb();
                        // L0:
                        let lhs = self.visit_expr(lhs).unwrap().into_int_value();
                        self.build_conditional_branch(lhs, l2, l1).unwrap();
                        // L1:
                        self.position_at_end(l1);
                        let rhs = self.visit_expr(rhs).unwrap().into_int_value();
                        self.build_conditional_branch(rhs, l2, l3).unwrap();
                        // L2:
                        self.position_at_end(l2);
                        self.build_store(t, self.const_bool(true)).unwrap();
                        self.build_unconditional_branch(l3).unwrap();
                        // L3:
                        self.position_at_end(l3);

                        return Some(self.build_load(bool_ty, t, "").unwrap());
                    }
                    _ => {}
                }

                let lhs = self.visit_expr(lhs).unwrap();
                let rhs = self.visit_expr(rhs).unwrap();

                Some(match binop {
                    BinaryOp::Add => self
                        .builder
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "")
                        .unwrap()
                        .into(),
                    BinaryOp::Sub => self
                        .builder
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "")
                        .unwrap()
                        .into(),
                    BinaryOp::Mul => self
                        .builder
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "")
                        .unwrap()
                        .into(),
                    BinaryOp::Le => self
                        .build_int_compare(
                            IntPredicate::ULE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "",
                        )
                        .unwrap()
                        .into(),
                    BinaryOp::Ge => self
                        .build_int_compare(
                            IntPredicate::UGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "",
                        )
                        .unwrap()
                        .into(),
                    _ => panic!("todo: {binop:?}"),
                })
            }
            ExprKind::Return(expr) => {
                let val = expr.map(|expr| self.visit_expr(expr)).flatten();

                self.build_return(val.as_ref().map(|v| v as &dyn BasicValue))
                    .unwrap();

                None
            }
            ExprKind::If(cond, then, r#else) => {
                // if (cond) { then } else { else }

                let mut last = None;
                let mut returned = false;

                let cond = self.visit_expr(cond).unwrap().into_int_value();

                let t = if expr.ty.is_unit() {
                    None
                } else {
                    Some({
                        let ty = self.ty_to_llvm(&expr.ty).unwrap();
                        (self.build_alloca(ty, "").unwrap(), ty)
                    })
                };

                if let Some(r#else) = r#else {
                    let then_block = self.append_bb();
                    let else_block = self.append_bb();
                    let final_block = self.append_bb();

                    self.build_conditional_branch(cond, then_block, else_block)
                        .unwrap();

                    // then:
                    self.position_at_end(then_block);
                    for stmt in then.stmts {
                        last = self.visit_stmt(stmt);

                        if Self::check_ret(stmt) {
                            returned = true;
                            break;
                        }
                    }

                    if !returned {
                        if let Some(t) = t {
                            self.build_store(t.0, last.unwrap()).unwrap();
                        }

                        self.build_unconditional_branch(final_block).unwrap();
                    }
                    returned = false;

                    // else:
                    self.position_at_end(else_block);
                    for stmt in r#else.stmts {
                        last = self.visit_stmt(stmt);

                        if Self::check_ret(stmt) {
                            returned = true;
                            break;
                        }
                    }

                    if !returned {
                        if let Some(t) = t {
                            self.build_store(t.0, last.unwrap()).unwrap();
                        }

                        self.build_unconditional_branch(final_block).unwrap();
                    }
                    returned = false;

                    // final:
                    self.position_at_end(final_block);
                } else {
                    let then_block = self.append_bb();
                    let final_block = self.append_bb();

                    self.build_conditional_branch(cond, then_block, final_block)
                        .unwrap();

                    // then:
                    self.position_at_end(then_block);
                    for stmt in then.stmts {
                        last = self.visit_stmt(stmt);

                        if Self::check_ret(stmt) {
                            returned = true;
                            break;
                        }
                    }
                    if !returned {
                        self.build_unconditional_branch(final_block).unwrap();
                    }

                    // final:
                    self.position_at_end(final_block);
                }

                t.map(|(ptr, ty)| self.build_load(ty, ptr, "").unwrap())
            }
            _ => todo!(),
        }
    }

    fn check_ret(stmt: &Stmt) -> bool {
        if let Stmt::Expr(expr) = stmt {
            matches!(expr.kind, ExprKind::Return(_))
        } else {
            false
        }
    }

    fn append_bb(&self) -> BasicBlock<'ctx> {
        self.context
            .append_basic_block(self.current_fn.unwrap(), "")
    }

    fn const_bool(&self, bool: bool) -> IntValue<'ctx> {
        self.context.bool_type().const_int(bool as u64, false)
    }

    fn find_fn(&mut self, sym: &Symbol) -> Option<FunctionValue<'ctx>> {
        for ctx in &self.fns {
            let Some(f) = ctx.get(sym) else {
                continue;
            };

            return Some(*f);
        }

        None
    }

    fn current_fns(&mut self) -> &mut HashMap<Symbol, FunctionValue<'ctx>> {
        self.fns.last_mut().unwrap()
    }

    fn current_scope(&mut self) -> &mut HashMap<Symbol, (PointerValue<'ctx>, Type)> {
        self.scopes.last_mut().unwrap()
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.fns.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
        self.fns.pop();
    }

    fn ty_to_llvm(&self, ty: &Type) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            Type::AbstractInt | Type::AbstractFloat => {
                panic!("Found abstract type");
            }
            Type::Refined(sym) => {
                if sym == &UNIT {
                    return None;
                }
                if sym == &BOOLEAN {
                    return Some(self.context.bool_type().into());
                }
                if [INT8, UINT8].contains(sym) {
                    return Some(self.context.i8_type().into());
                }
                if [INT16, UINT16].contains(sym) {
                    return Some(self.context.i16_type().into());
                }
                if [INT32, UINT32].contains(sym) {
                    return Some(self.context.i32_type().into());
                }
                if [INT64, UINT64].contains(sym) {
                    return Some(self.context.i64_type().into());
                }

                if sym == &FLOAT32 {
                    return Some(self.context.f32_type().into());
                }
                if sym == &FLOAT64 {
                    return Some(self.context.f64_type().into());
                }

                panic!()
            }
            _ => panic!(),
        }
    }

    fn lit_to_llvm(&self, val: &Literal, ty: &Type) -> BasicValueEnum<'ctx> {
        match *val {
            Literal::Boolean(bool) => self
                .context
                .bool_type()
                .const_int(bool as u64, false)
                .into(),
            Literal::Int(int) => {
                let signed = ty.is_sint();

                let ty = self.ty_to_llvm(ty).unwrap();

                ty.into_int_type().const_int(int as _, signed).into()
            }
            Literal::Float(f) => {
                let ty = self.ty_to_llvm(ty).unwrap();

                ty.into_float_type().const_float(f).into()
            }
        }
    }
}

impl<'ctx> Deref for CodegenContext<'ctx> {
    type Target = Builder<'ctx>;
    fn deref(&self) -> &Self::Target {
        &self.builder
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let str = include_str!("../test.krab");
    let mut parser = Parser::new(str);

    with_global_session_init(|| {
        let stmts = parser.parse_stmt_list();

        let ctx = TirCtx::default();
        let mut lowering = LoweringContext::new(&ctx);

        // global scope
        lowering.enter_scope();
        let tir = lowering.lowering_stmt_list(&stmts);
        lowering.exit_scope();

        // dbg!(tir);

        let context = Context::create();
        let module = context.create_module("test.krab");

        let mut codegen = CodegenContext::new(&context, module, context.create_builder());

        codegen.enter_scope();
        for stmt in tir {
            codegen.visit_stmt(stmt);
        }
        codegen.exit_scope();

        Target::initialize_all(&InitializationConfig::default());

        let triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();

        let target = Target::from_triple(&triple).unwrap();

        let tm = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        let td = tm.get_target_data();
        let layout = td.get_data_layout();

        codegen.module.set_data_layout(&layout);
        codegen
            .module
            .run_passes(
                "mem2reg,adce,bdce,aa-eval,dce,no-op-function",
                &tm,
                PassBuilderOptions::create(),
            )
            .unwrap();

        codegen.module.print_to_stderr();

        tm.write_to_file(
            &codegen.module,
            FileType::Assembly,
            &Path::new("../output/test.s"),
        )?;

        Ok(())
    })
}
