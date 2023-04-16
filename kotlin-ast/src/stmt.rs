use crate::decl::DeclStmt;
use crate::expr::ExprStmt;
use crate::Ident;

#[derive(Debug)]
pub enum Stmt {
    Decl(DeclStmt),
    Expr(ExprStmt),
    Assign(AssignStmt),
    Empty,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Option<Ident>,
    pub kind: AssignKind,
}

#[derive(Debug)]
pub enum AssignKind {
    Decl,
    Init(ExprStmt),
}
