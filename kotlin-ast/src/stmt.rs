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
    Assign(AssignStmt),
    Empty,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: ExprStmt,
    pub body: Block,
}

#[derive(Debug)]
pub struct ForStmt {
    pub bind: Ident,
    pub target: ExprStmt,
    pub body: Block,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub id: Ident,
    pub expr: ExprStmt,
    pub op: Option<AssignOp>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignOp {
    /// "+="
    AddAssign,
    /// "-="
    SubAssign,
    /// "*="
    MulAssign,
    /// "/="
    DivAssign,
    /// "%="
    RemAssign,
}
