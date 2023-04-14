use crate::expr::ExprStmt;

#[derive(Debug)]
pub enum Stmt {
    Assign(AssignStmt),
    Block(BlockStmt),
    Expr(ExprStmt),
}

#[derive(Debug)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct AssignStmt {}
