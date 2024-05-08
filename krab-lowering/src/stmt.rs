use crate::LoweringContext;
use kotlin_ast::decl::{DeclStmt as AstDeclStmt, VarKind};
use kotlin_ast::stmt::Stmt as AstStmt;
use kotlin_span::{Ident, BOOLEAN, UNIT};
use krab_tir::stmt::{Decl, Stmt};
use krab_tir::ty::Type;

impl<'tir> LoweringContext<'tir> {
    pub fn lowering_stmt_list(&mut self, stmts: &'tir [AstStmt]) -> &'tir [Stmt<'tir>] {
        let mut t = Vec::with_capacity(stmts.len());

        for stmt in stmts {
            t.push(self.lowering_stmt(stmt));
        }

        let list = self.stmt_alloc.alloc_extend(t);

        for stmt in &mut *list {
            match stmt {
                Stmt::Decl(Decl::Var(name, init)) => {
                    if let Some(expr) = init {
                        let actual = self.find_ty(*name).unwrap();
                        expr.ty = Type::lowering(expr.ty.clone(), actual).unwrap();
                    }
                }
                _ => {}
            }
        }

        list
    }

    pub fn lowering_stmt(&mut self, stmt: &'tir AstStmt) -> Stmt<'tir> {
        match stmt {
            AstStmt::Decl(decl) => Stmt::Decl(self.lowering_decl_stmt(decl)),
            AstStmt::Expr(expr) => Stmt::Expr(self.lowering_expr(expr, None)),
            AstStmt::While(r#while) => {
                let expr = self.lowering_expr(&r#while.cond, Some(Type::Refined(BOOLEAN)));
                let body = self.lowering_block(&r#while.body);

                Stmt::While(expr, body)
            }
            AstStmt::Assign(assign) => {
                let (ty, a) = self.current_scope().get(&assign.id.symbol()).unwrap();
                let (ty, a) = (ty.clone(), *a);
                if a {
                    let expr = self.lowering_expr(&assign.expr, Some(ty));

                    Stmt::Assign(assign.id, expr, assign.op)
                } else {
                    panic!("not assignable");
                }
            }
            _ => todo!(),
        }
    }

    pub fn lowering_decl_stmt(&mut self, stmt: &'tir AstDeclStmt) -> Decl<'tir> {
        match stmt {
            AstDeclStmt::Fun(fun) => {
                self.enter_scope();

                let ret_ty = fun.ret_type.map(Ident::symbol).unwrap_or(UNIT);
                let ret_ty = Type::Refined(ret_ty);

                self.curr_ret = Some(ret_ty.clone());

                let mut args_ty = vec![];

                let scope = self.current_scope();

                for arg in &fun.args {
                    let ty = Type::Refined(arg.ty.symbol());
                    scope.insert(arg.name.symbol(), (ty.clone(), false));
                    args_ty.push((arg.name, ty));
                }

                let fn_ty = Type::Callable(
                    ret_ty.clone().into(),
                    args_ty.iter().cloned().map(|d| d.1).collect(),
                );
                scope.insert(fun.name.symbol(), (fn_ty.clone(), false));

                let body = self.lowering_block(&fun.body);

                self.curr_ret = None;

                self.exit_scope();

                self.current_scope()
                    .insert(fun.name.symbol(), (fn_ty, false));

                Decl::Fun(
                    fun.name,
                    ret_ty,
                    self.type_id_alloc.alloc_extend(args_ty),
                    body,
                )
            }
            AstDeclStmt::Variable(var) => {
                let (init, ty) = match &var.kind {
                    VarKind::Decl => {
                        assert!(
                            var.ty.is_some(),
                            "var decl without init must declared with type!"
                        );

                        (None, Type::Refined(var.ty.unwrap().symbol()))
                    }
                    VarKind::Init(expr) => {
                        let expr =
                            self.lowering_expr(expr, var.ty.map(|id| Type::Refined(id.symbol())));
                        let ty = expr.ty.clone();
                        (Some(expr), ty)
                    }
                };

                let scope = self.current_scope();
                scope.insert(var.name.symbol(), (ty, var.mutable));

                Decl::Var(var.name, init)
            }
            _ => todo!(),
        }
    }
}
