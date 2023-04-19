use clap::Arg;
use kotlin_ast::decl::{DeclStmt, FunDecl};
use kotlin_ast::expr::{BinaryExpr, BinaryOp, CallExpr, ExprStmt, LiteralExpr, ReturnExpr, UnaryExpr, UnaryOp};
use kotlin_ast::decl::{VarKind,VariableDecl};
use kotlin_ast::stmt::Stmt;
use kotlin_parse::Parser;
use kotlin_span::symbol::Symbol;
use kotlin_span::with_global_session_init;
use llvm_sys::core::{LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildICmp, LLVMBuildLoad2, LLVMBuildMul, LLVMBuildNot, LLVMBuildRet, LLVMBuildStore, LLVMBuildSub, LLVMBuildTrunc, LLVMConstInt, LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeModule, LLVMDumpModule, LLVMFunctionType, LLVMGetCalledFunctionType, LLVMGetParam, LLVMGetReturnType, LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMPositionBuilderAtEnd, LLVMVoidTypeInContext};
use llvm_sys::prelude::{
    LLVMBasicBlockRef, LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMTypeRef, LLVMValueRef,
};
use llvm_sys::LLVMIntPredicate;
use std::collections::HashMap;
use std::ffi::CString;
use std::path::PathBuf;
use std::ptr::null_mut;
use kotlin_ast::block::Block;

fn main() {
    let cmd = clap::Command::new("kotlincn")
        .arg(
            Arg::new("compile")
                .short('c')
                .value_parser(clap::value_parser!(PathBuf))
                .help("compile a file")
                .required(true),
        )
        .name("Kotlin Compiler for Native")
        .version("0.1")
        .about("Kotlin Compiler for Native")
        .help_template("{name} ({version})");

    let matches = cmd.get_matches();
    let path: &PathBuf = matches.get_one("compile").unwrap();
    let file = std::fs::read_to_string(path).unwrap();

    unsafe {
        with_global_session_init(|| {
            let mut parser = Parser::new(&file);

            let mut vis = Visitor::new();
            vis.scope(|vis| {
                for stmt in parser.parse_stmt_list() {
                    vis.visit_stmt(stmt);
                }
            });
            for err in parser.errors() {
                println!("Error: {}", err);
            }

            vis.dump();
        });
    }
}

const NUL_CSTR: *const i8 = "\0".as_ptr() as *const i8;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Type {
    Boolean,
    Int8,
    Int16,
    Int32,
    Int64,
    Unit,
    Function(LLVMTypeRef),
}

impl Type {
    pub unsafe fn get_llvm_type(self, ctx: LLVMContextRef) -> LLVMTypeRef {
        match self {
            Self::Boolean => LLVMInt1TypeInContext(ctx),
            Self::Int8 => LLVMInt8TypeInContext(ctx),
            Self::Int16 => LLVMInt16TypeInContext(ctx),
            Self::Int32 => LLVMInt32TypeInContext(ctx),
            Self::Int64 => LLVMInt64TypeInContext(ctx),
            Self::Unit => LLVMVoidTypeInContext(ctx),
            _ => unreachable!()
        }
    }

    pub fn compare(a: Self, b: Self) -> Self {
        match (a, b) {
            (Self::Boolean, Self::Boolean) => Self::Boolean,
            (_, Self::Boolean) | (Self::Boolean, _) => panic!("boolean type"),
            (Self::Unit, _) | (_, Self::Unit) => panic!("not a valid type"),
            (Self::Int8, el) => el,
            (el, Self::Int8) => el,
            (Self::Int16, el) => el,
            (el, Self::Int16) => el,
            (Self::Int32, el) => el,
            (el, Self::Int32) => el,
            _ => Self::Int64,
        }
    }

    pub fn from_symbol(sym: Symbol) -> Self {
        use kotlin_span::*;
        match sym {
            UNIT => Self::Unit,
            BOOLEAN => Self::Boolean,
            INT8 => Self::Int8,
            INT16 => Self::Int16,
            INT32 => Self::Int32,
            INT64 => Self::Int64,
            _ => panic!("unknown type"),
        }
    }

    pub fn signed(&self) -> bool {
        matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Int8 | Self::Int16 | Self::Int32 | Self::Int64)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct Value {
    llvm: LLVMValueRef,
    ty: Type,
}

pub struct Context {
    vars: HashMap<Symbol, (Value, bool)>,
    bb: LLVMBasicBlockRef,
}

pub struct Visitor {
    ctx: LLVMContextRef,
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    var_maps: Vec<HashMap<Symbol, (Value, bool)>>,
    globals: HashMap<Symbol, (Value, bool)>,
    curr_fn: LLVMValueRef,
    block: LLVMBasicBlockRef,
}

impl Visitor {
    pub unsafe fn new() -> Self {
        let ctx = LLVMContextCreate();
        let module = LLVMModuleCreateWithNameInContext("main.kt\0".as_ptr() as _, ctx);
        let builder = LLVMCreateBuilderInContext(ctx);

        Self {
            ctx,
            module,
            builder,
            var_maps: Vec::with_capacity(10),
            globals: HashMap::new(),
            curr_fn: null_mut(),
            block: null_mut()
        }
    }

    pub unsafe fn visit_expr(&mut self, expr: ExprStmt, raw: bool) -> Value {
        match expr.peel_paren() {
            ExprStmt::Lit(LiteralExpr::Boolean(val)) => {
                let i1ty = LLVMInt1TypeInContext(self.ctx);
                let ll = LLVMConstInt(i1ty, val as _, 0);
                Value {
                    llvm: ll,
                    ty: Type::Boolean,
                }
            }
            ExprStmt::Return(ret) => self.visit_return_expr(ret).unwrap(),
            ExprStmt::Binary(bin_expr) => self.visit_binary_expr(bin_expr),
            ExprStmt::Ident(id) => {
                let (var, init) = self.find_var(id.symbol()).unwrap();
                if !init {
                    Self::uninit();
                }
                if raw {
                    return var;
                }
                let ll = LLVMBuildLoad2(
                    self.builder,
                    var.ty.get_llvm_type(self.ctx),
                    var.llvm,
                    NUL_CSTR,
                );
                Value {
                    llvm: ll,
                    ty: var.ty,
                }
            }
            ExprStmt::Call(call) => self.visit_call_expr(call),
            ExprStmt::Unary(unary) => self.visit_unary_expr(unary),
            ExprStmt::Paren(_) => unreachable!(),
            expr => unimplemented!("{:?}", expr),
        }
    }

    pub unsafe fn visit_call_expr(&mut self, CallExpr { expr, args }: CallExpr) -> Value {
        let expr = self.visit_expr(*expr, true);
        assert!(matches!(expr.ty, Type::Function(_)));
        let Type::Function(funty) = expr.ty else {
            unreachable!();
        };
        let mut values: Vec<LLVMValueRef> = args.into_iter().map(|arg| self.visit_expr(arg, false).llvm).collect();
        let arg_len = values.len();

        let ret = Type::Boolean.get_llvm_type(self.ctx);
        let ty = if Type::Boolean.get_llvm_type(self.ctx) == ret {
            Type::Boolean
        } else {
            Type::Boolean
        };

        Value {
            llvm: LLVMBuildCall2(
                self.builder,
                funty,
                expr.llvm,
                values.as_mut_ptr(),
                arg_len as u32,
                NUL_CSTR
            ),
            ty
        }
    }

    pub unsafe fn visit_unary_expr(&mut self, UnaryExpr { op, expr }: UnaryExpr) -> Value {
        let (vall, ty) = match op {
            UnaryOp::PreInc => {
                let val = self.visit_expr(*expr, true);
                let one = LLVMConstInt(val.ty.get_llvm_type(self.ctx), 1, val.ty.signed() as i32);
                assert!(val.ty.is_integer());
                let res = LLVMBuildAdd(self.builder, val.llvm, one, NUL_CSTR);
                LLVMBuildStore(self.builder, res, val.llvm);
                (val.llvm, val.ty)
            }
            UnaryOp::PreDec => {
                let val = self.visit_expr(*expr, true);
                let one = LLVMConstInt(val.ty.get_llvm_type(self.ctx), 1, val.ty.signed() as i32);
                assert!(val.ty.is_integer());
                let res = LLVMBuildSub(self.builder, val.llvm, one, NUL_CSTR);
                LLVMBuildStore(self.builder, res, val.llvm);
                (val.llvm, val.ty)
            }
            UnaryOp::Not => {
                let val = self.visit_expr(*expr, false);
                assert_eq!(val.ty, Type::Boolean);
                (LLVMBuildNot(self.builder, val.llvm, NUL_CSTR), val.ty)
            }
            _ => unimplemented!(),
        };

        Value { llvm: vall, ty }
    }

    pub unsafe fn visit_binary_expr(&mut self, BinaryExpr { op, lhs, rhs }: BinaryExpr) -> Value {
        let lhs = self.visit_expr(*lhs, false);
        match op {
            BinaryOp::And => {
                // a && b ->
                // if a goto L1 else L3
                // L1:
                // if b goto L2 else L3
                // L2:
                // t = true
                // goto L3
                // L3:
                // *code*
                let ty = lhs.ty;

                assert_eq!(ty, Type::Boolean);
                let boolty = ty.get_llvm_type(self.ctx);
                let tmp = LLVMBuildAlloca(self.builder, boolty, NUL_CSTR);
                LLVMBuildStore(self.builder, LLVMConstInt(boolty, 0, 0), tmp);
                let l1 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                let l2 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                let l3 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                LLVMBuildCondBr(self.builder, lhs.llvm, l1, l3);
                // L1
                LLVMPositionBuilderAtEnd(self.builder, l1);
                let rhs = self.visit_expr(*rhs, false);
                assert_eq!(rhs.ty, Type::Boolean);
                LLVMBuildCondBr(self.builder, rhs.llvm, l2, l3);
                // L2
                LLVMPositionBuilderAtEnd(self.builder, l2);
                LLVMBuildStore(self.builder, LLVMConstInt(boolty, 1, 0), tmp);
                LLVMBuildBr(self.builder, l3);
                // L3
                LLVMPositionBuilderAtEnd(self.builder, l3);
                return Value {
                    llvm: LLVMBuildLoad2(self.builder, boolty, tmp, NUL_CSTR),
                    ty,
                };
            }
            BinaryOp::Or => {
                // a || b ->
                // if a goto L2 else L1
                // L1:
                // if b goto L2 else L3
                // L2:
                // t = true
                // goto L3
                // L3:
                // *code*
                let ty = lhs.ty;

                assert_eq!(ty, Type::Boolean);
                let boolty = ty.get_llvm_type(self.ctx);
                let tmp = LLVMBuildAlloca(self.builder, boolty, NUL_CSTR);
                LLVMBuildStore(self.builder, LLVMConstInt(boolty, 0, 0), tmp);
                let l1 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                let l2 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                let l3 = LLVMAppendBasicBlockInContext(self.ctx, self.curr_fn, NUL_CSTR);
                LLVMBuildCondBr(self.builder, lhs.llvm, l2, l1);
                // L1
                LLVMPositionBuilderAtEnd(self.builder, l1);
                let rhs = self.visit_expr(*rhs, false);
                assert_eq!(rhs.ty, Type::Boolean);
                LLVMBuildCondBr(self.builder, rhs.llvm, l2, l3);
                // L2
                LLVMPositionBuilderAtEnd(self.builder, l2);
                LLVMBuildStore(self.builder, LLVMConstInt(boolty, 1, 0), tmp);
                LLVMBuildBr(self.builder, l3);
                // L3
                LLVMPositionBuilderAtEnd(self.builder, l3);
                return Value {
                    llvm: LLVMBuildLoad2(self.builder, boolty, tmp, NUL_CSTR),
                    ty,
                };
            }
            _ => {}
        }

        let rhs = self.visit_expr(*rhs, false);
        let (lhs, rhs, ty) = self.trunc_value(lhs, rhs);

        match op {
            BinaryOp::Add => Value {
                llvm: LLVMBuildAdd(
                    self.builder,
                    lhs,
                    rhs,
                    NUL_CSTR
                ),
                ty,
            },
            BinaryOp::Sub => Value {
                llvm: LLVMBuildSub(
                    self.builder,
                    lhs,
                    rhs,
                    NUL_CSTR
                ),
                ty,
            },
            BinaryOp::Mul => Value {
                llvm: LLVMBuildMul(
                    self.builder,
                    lhs,
                    rhs,
                    NUL_CSTR
                ),
                ty,
            },
            BinaryOp::Eq => Value {
                llvm: LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    lhs,
                    rhs,
                    NUL_CSTR,
                ),
                ty,
            },
            BinaryOp::Ne => Value {
                llvm: LLVMBuildICmp(
                    self.builder,
                    LLVMIntPredicate::LLVMIntNE,
                    lhs,
                    rhs,
                    NUL_CSTR,
                ),
                ty,
            },
            _ => unimplemented!(),
        }
    }

    pub unsafe fn visit_return_expr(
        &mut self,
        ReturnExpr { expr, at: _ }: ReturnExpr,
    ) -> Option<Value> {
        let val = expr.map(|expr| self.visit_expr(*expr, false))?;

        Some(Value {
            llvm: LLVMBuildRet(self.builder, val.llvm),
            ty: Type::Unit,
        })
    }

    pub unsafe fn visit_decl(&mut self, decl: DeclStmt) {
        match decl {
            DeclStmt::Variable(var) => self.visit_variable_decl(var),
            DeclStmt::Fun(fun) => {
                self.visit_fun_decl(fun);
            },
            decl => unimplemented!("{decl:?}")
        }
    }

    pub unsafe fn visit_variable_decl(&mut self, ass: VariableDecl) {
        let ty = ass.ty.map(|id| Type::from_symbol(id.symbol()));

        match ass.kind {
            VarKind::Decl => {
                self.alloca_var(ass.name.symbol(), ty.unwrap(), None);
            }
            VarKind::Init(expr) => {
                let expr = self.visit_expr(expr, false);
                assert_eq!(expr.ty, ty.unwrap_or(expr.ty));
                self.alloca_var(ass.name.symbol(), expr.ty, Some(expr.llvm));
            }
        }
    }

    pub unsafe fn visit_fun_decl(&mut self, FunDecl { name, args, body, ret_type }: FunDecl) {
        let use_globals = self.var_maps.len() == 0;
        let prev_scope = self.var_maps.len().saturating_sub(1);
        self.new_scope();

        let sym = name.symbol();
        let str = sym.as_str();
        let cstring = CString::new(str).unwrap();
        let ret = ret_type.map(|id| Type::from_symbol(id.symbol()))
            .unwrap_or(Type::Unit);

        let mut args_ll: Vec<LLVMTypeRef> = args.iter().map(|arg| Type::from_symbol(arg.ty.symbol())
            .get_llvm_type(self.ctx)
        ).collect();

        let arg_len = args_ll.len();

        let funty = LLVMFunctionType(ret.get_llvm_type(self.ctx), args_ll.as_mut_ptr(), arg_len as u32, 0);
        let fun = LLVMAddFunction(self.module, cstring.as_ptr(), funty);
        let prev = self.curr_fn;
        self.curr_fn = fun;
        let v = (Value { llvm: fun, ty: Type::Function(funty) }, true);
        if let Some(map) = self.var_maps.get_mut(prev_scope) {
            map.insert(name.symbol(), v);
        }
        let map = self.var_maps.last_mut().unwrap();
        if use_globals {
            self.globals.insert(name.symbol(), v);
        }
        map.insert(name.symbol(), v);
        for (i, arg) in args.into_iter().enumerate() {
            map.insert(arg.name.symbol(), (Value {
                llvm: LLVMGetParam(fun, i as u32),
                ty: Type::from_symbol(arg.ty.symbol())
            }, true));
        }
        let entry = LLVMAppendBasicBlockInContext(self.ctx, fun, NUL_CSTR);
        let prev_entry = self.block;
        self.block = entry;

        LLVMPositionBuilderAtEnd(self.builder, entry);
        let Block {
            stmts, span: _
        } = body;
        for stmt in stmts {
            self.visit_stmt(stmt);
        }

        self.curr_fn = prev;
        self.block = prev_entry;
        LLVMPositionBuilderAtEnd(self.builder, prev_entry);
        self.end_scope();
    }

    pub unsafe fn visit_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.visit_expr(expr, false);
            }
            Stmt::Decl(decl) => {
                self.visit_decl(decl);
            }

            _ => {}
        }
    }

    unsafe fn trunc_value(&mut self, lhs: Value, rhs: Value) -> (LLVMValueRef, LLVMValueRef, Type) {
        let ty = Type::compare(lhs.ty, rhs.ty);
        let llty = ty.get_llvm_type(self.ctx);

        if lhs.ty == rhs.ty {
            return (lhs.llvm, rhs.llvm, ty);
        }

        let val = if ty == lhs.ty { rhs.llvm } else { lhs.llvm };

        let lhs = LLVMBuildTrunc(self.builder, val, llty, NUL_CSTR);
        (lhs, rhs.llvm, ty)
    }

    pub fn scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.new_scope();
        f(self);
        self.end_scope();
    }

    pub fn new_scope(&mut self) {
        self.var_maps.push(HashMap::default());
    }

    pub fn end_scope(&mut self) {
        self.var_maps.pop();
    }

    pub fn find_var(&self, sym: Symbol) -> Option<(Value, bool)> {
        for map in self.var_maps.iter().rev() {
            if let Some(&var) = map.get(&sym) {
                return Some(var);
            }
        }

        if let Some(&var) = self.globals.get(&sym) {
            return Some(var);
        }

        None
    }

    pub unsafe fn alloca_var(
        &mut self,
        sym: Symbol,
        ty: Type,
        assign: Option<LLVMValueRef>,
    ) -> Value {
        let map = self.var_maps.last_mut().unwrap();
        let ll = LLVMBuildAlloca(self.builder, ty.get_llvm_type(self.ctx), NUL_CSTR);
        if let Some(ass) = assign {
            LLVMBuildStore(self.builder, ass, ll);
        }

        let val = Value { llvm: ll, ty };
        map.insert(sym, (val, assign.is_some()));
        val
    }

    pub unsafe fn primary_type(&self) -> LLVMTypeRef {
        LLVMInt32TypeInContext(self.ctx)
    }

    pub unsafe fn dump(&self) {
        LLVMDumpModule(self.module);
    }

    pub fn uninit() -> ! {
        panic!("use of uninitialized value");
    }
}

impl Drop for Visitor {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.ctx);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Visitor;
    use kotlin_parse::Parser;
    use kotlin_span::with_global_session_init;

    #[test]
    fn logic() {
        let s = r#"
        fun main(): Unit {
            fun a(): Boolean {
                val da = true
                val b = false
                return da || (da && b) || b
            }

            var da = a()
        }
        "#;
        unsafe {
            with_global_session_init(|| {
                let mut parser = Parser::new(s);

                let mut vis = Visitor::new();
                for stmt in parser.parse_stmt_list() {
                    vis.visit_stmt(stmt);
                }
                for err in parser.errors() {
                    eprintln!("Error: {}", err);
                }
                vis.dump();
            });
        }
    }
}
