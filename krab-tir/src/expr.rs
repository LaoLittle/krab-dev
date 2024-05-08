use crate::block::Block;
use crate::ty::Type;
use kotlin_ast::expr::{BinaryOp, UnaryOp};
use kotlin_span::Ident;

#[derive(Debug)]
pub struct Expr<'tir> {
    pub kind: ExprKind<'tir>,
    pub ty: Type,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ExprKind<'tir> {
    Variable(Ident),
    Call(&'tir Expr<'tir>, &'tir [Expr<'tir>]),
    Lit(Literal),
    If(&'tir Expr<'tir>, Block<'tir>, Option<Block<'tir>>),
    Unary(UnaryOp, &'tir Expr<'tir>),
    Binary(BinaryOp, &'tir Expr<'tir>, &'tir Expr<'tir>),
    Return(Option<&'tir Expr<'tir>>),
}

#[derive(Debug, Copy, Clone)]
pub enum Literal {
    Boolean(bool),
    Int(u128),
    Float(f64),
}
