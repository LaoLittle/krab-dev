use crate::stmt::BlockStmt;
use crate::Span;

#[derive(Debug)]
pub enum Decl {
    Function(FunctionDecl),
}

#[derive(Debug)]
pub struct FunctionDecl {
    name: Span,
    body: BlockStmt,
}
