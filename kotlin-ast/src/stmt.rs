use crate::decl::DeclStmt;
use crate::expr::ExprStmt;
use crate::Ident;

#[derive(Debug)]
pub enum Stmt {
    Decl(DeclStmt),
    Expr(ExprStmt),
    Empty,
}
