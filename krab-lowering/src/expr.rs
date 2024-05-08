use smallvec::SmallVec;

use kotlin_ast::expr::{
    BinaryExpr, BinaryOp, CallExpr, ExprStmt, FloatTy, IfExpr, IntTy, LiteralExpr, UnaryExpr,
    UnaryOp,
};
use kotlin_span::{BOOLEAN, FLOAT32, INT64, UNIT};
use krab_tir::expr::{Expr, ExprKind, Literal};
use krab_tir::stmt::Stmt;
use krab_tir::ty::Type;

use crate::LoweringContext;

fn peel_paren_ref(mut expr: &ExprStmt) -> &ExprStmt {
    while let ExprStmt::Paren(inner) = expr {
        expr = &inner;
    }

    expr
}

impl<'tir> LoweringContext<'tir> {
    pub fn lowering_expr(&mut self, expr: &'tir ExprStmt, refine: Option<Type>) -> Expr<'tir> {
        match peel_paren_ref(expr) {
            ExprStmt::Ident(id) => {
                let id = *id;

                let var_ty = self.get_ty_mut(id);

                if let Some(ty) = refine {
                    *var_ty = Type::lowering(var_ty.clone(), ty).expect("type mismatched");
                }

                let c = var_ty.clone();

                Expr {
                    kind: ExprKind::Variable(id),
                    ty: c,
                }
            }
            ExprStmt::Call(CallExpr { expr, args }) => {
                let expr = self.lowering_expr(expr, None);

                let Type::Callable(fn_ret, fn_args) = expr.ty.clone() else {
                    panic!("Not a callable");
                };

                assert_eq!(args.len(), fn_args.len());

                let mut temp = SmallVec::<Expr, 16>::new();

                for i in 0..args.len() {
                    let fn_arg_ty = fn_args[i].clone();
                    let in_arg = &args[i];

                    let expr = self.lowering_expr(in_arg, Some(fn_arg_ty));
                    temp.push(expr);
                }

                let expr = self.expr_alloc.alloc(expr);
                let args = self.expr_alloc.alloc_extend(temp);

                Expr {
                    kind: ExprKind::Call(expr, args),
                    ty: *fn_ret,
                }
            }
            ExprStmt::If(IfExpr { cond, then, r#else }) => {
                let cond = self.lowering_expr(cond, Some(Type::Refined(BOOLEAN)));
                let cond = self.expr_alloc.alloc(cond);
                let then = self.lowering_block(then);
                let r#else = r#else.as_ref().map(|b| self.lowering_block(b));

                let mut l = Type::Refined(UNIT);
                let mut r = Type::Refined(UNIT);

                if let Some(Stmt::Expr(expr)) = then.stmts.last() {
                    l = expr.ty.clone();
                }

                match &r#else {
                    None => l = Type::Refined(UNIT),
                    Some(b) => {
                        if let Some(Stmt::Expr(expr)) = b.stmts.last() {
                            r = expr.ty.clone();
                        }
                    }
                }

                let ty = Type::lowering(l, r).unwrap();

                Expr {
                    kind: ExprKind::If(cond, then, r#else),
                    ty,
                }
            }
            ExprStmt::Lit(lit) => match lit {
                LiteralExpr::Boolean(bool) => {
                    let ty = Type::Refined(BOOLEAN);

                    if let Some(refined) = refine {
                        Type::lowering(ty.clone(), refined).unwrap();
                    }

                    Expr {
                        kind: ExprKind::Lit(Literal::Boolean(*bool)),
                        ty,
                    }
                }
                LiteralExpr::Integer { int, ty } => {
                    let mut ty = ty
                        .as_ref()
                        .map(|intty| match intty {
                            IntTy::Long => Type::Refined(INT64),
                            _ => Type::AbstractInt,
                        })
                        .unwrap_or(Type::AbstractInt);

                    if let Some(refined) = refine {
                        ty = Type::lowering(ty, refined).expect("type mismatched");
                    }

                    Expr {
                        kind: ExprKind::Lit(Literal::Int(*int)),
                        ty,
                    }
                }
                LiteralExpr::Float { float, ty } => {
                    let mut ty = ty
                        .as_ref()
                        .map(|floatty| match floatty {
                            FloatTy::Float32 => Type::Refined(FLOAT32),
                        })
                        .unwrap_or(Type::AbstractFloat);

                    if let Some(refined) = refine {
                        ty = Type::lowering(ty, refined).unwrap();
                    }

                    Expr {
                        kind: ExprKind::Lit(Literal::Float(*float)),
                        ty,
                    }
                }
                _ => todo!(),
            },
            ExprStmt::Unary(UnaryExpr { op, expr }) => {
                let mut expr = self.lowering_expr(expr, None);

                if let Some(refined) = refine {
                    expr.ty = Type::lowering(expr.ty, refined).expect("type mismatched");
                }

                if matches!(op, UnaryOp::Negative) {}

                let expr = self.expr_alloc.alloc(expr);

                Expr {
                    kind: ExprKind::Unary(*op, expr),
                    ty: expr.ty.clone(),
                }
            }
            ExprStmt::Binary(BinaryExpr { op, lhs, rhs }) => {
                let trans = matches!(
                    op,
                    BinaryOp::Eq
                        | BinaryOp::Ne
                        | BinaryOp::Ge
                        | BinaryOp::Gt
                        | BinaryOp::Le
                        | BinaryOp::Lt
                );

                let lhs = self.lowering_expr(lhs, if trans { None } else { refine.clone() });
                let rhs = self.lowering_expr(rhs, Some(lhs.ty.clone())); // type lowered here.

                if matches!(op, BinaryOp::LAnd | BinaryOp::LOr) {
                    assert_eq!(lhs.ty, Type::Refined(BOOLEAN));
                    assert_eq!(rhs.ty, Type::Refined(BOOLEAN));
                }

                let mut ty = rhs.ty.clone();

                if let Some(refined) = refine {
                    let d = if trans {
                        Type::Refined(BOOLEAN)
                    } else {
                        ty.clone()
                    };

                    let d = Type::lowering(d, refined).expect("type mismatched");

                    if !trans {
                        ty = d;
                    }
                }

                if trans {
                    ty = rhs.ty.clone();
                }

                let lhs = self.expr_alloc.alloc(lhs);
                let rhs = self.expr_alloc.alloc(rhs);

                lhs.ty = ty.clone();
                rhs.ty = ty.clone();

                if let ExprKind::Variable(id) = &lhs.kind {
                    *self.get_ty_mut(*id) = lhs.ty.clone();
                }

                if let ExprKind::Variable(id) = &rhs.kind {
                    *self.get_ty_mut(*id) = rhs.ty.clone();
                }

                if trans {
                    ty = Type::Refined(BOOLEAN);
                }

                Expr {
                    kind: ExprKind::Binary(*op, lhs, rhs),
                    ty,
                }
            }
            ExprStmt::Return(expr) => {
                let mut ret = None;

                if let Some(expr) = &expr.expr {
                    let expr = self.lowering_expr(expr, self.curr_ret.clone());

                    ret = Some(&*self.expr_alloc.alloc(expr));
                }

                Expr {
                    kind: ExprKind::Return(ret),
                    ty: Type::Refined(UNIT),
                }
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use kotlin_ast::expr::ExprStmt;
    use kotlin_span::symbol::Symbol;
    use kotlin_span::{with_global_session_init, Ident, Span, UINT8};
    use krab_tir::ty::Type;

    use crate::{LoweringContext, TirCtx};

    #[test]
    fn lower() {
        with_global_session_init(|| {
            let ctx = TirCtx::default();
            let mut lowering = LoweringContext::new(&ctx);

            let expr = ExprStmt::call(
                ExprStmt::Ident(Ident::new(Symbol::intern("fuck"), Span::new(0, 0))),
                vec![],
            );

            lowering.enter_scope();
            lowering.current_scope().insert(
                Symbol::intern("fuck"),
                (Type::Callable(Type::Refined(UINT8).into(), vec![]), false),
            );

            let lowered = lowering.lowering_expr(&expr, None);

            lowering.exit_scope();

            panic!("{:#?}", lowered);
        });
    }
}
