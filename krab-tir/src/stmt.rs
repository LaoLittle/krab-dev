use crate::block::Block;
use crate::expr::Expr;
use crate::ty::Type;
use kotlin_ast::stmt::AssignOp;
use kotlin_span::Ident;

#[derive(Debug)]
pub enum Stmt<'tir> {
    Decl(Decl<'tir>),
    Expr(Expr<'tir>),
    While(Expr<'tir>, Block<'tir>),
    Assign(Ident, Expr<'tir>, Option<AssignOp>),
}

#[derive(Debug)]
pub enum Decl<'tir> {
    Fun(Ident, Type, &'tir [(Ident, Type)], Block<'tir>),
    Var(Ident, Option<Expr<'tir>>),
}
