use crate::block::Block;
use crate::decl::DeclStmt;
use crate::expr::ExprStmt;
use kotlin_span::Ident;

#[derive(Debug)]
pub enum Stmt {
    Decl(DeclStmt),
    Expr(ExprStmt),
    While(WhileStmt),
    For(ForStmt),
    Empty,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<ExprStmt>,
    pub body: Block,
}

#[derive(Debug)]
pub struct ForStmt {
    pub bind: Ident,
    pub target: Box<ExprStmt>,
    pub body: Block,
}
