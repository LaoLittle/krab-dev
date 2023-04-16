use crate::block::Block;
use crate::Ident;
use kotlin_span::Span;

#[derive(Debug)]
pub enum DeclStmt {
    Package(PackageDecl),
    Import(ImportDecl),
    Fun(FunDecl),
}

#[derive(Debug)]
pub enum PackageDecl {
    Valid(Span),
    InvalidName(usize),
}

#[derive(Debug)]
pub struct ImportDecl {
    pub package: Option<Span>,
    pub class: Ident,
}

#[derive(Debug)]
pub struct FunDecl {
    pub name: Ident,
    pub args: Vec<FunArg>,
    pub body: Block,
}

#[derive(Debug)]
pub struct FunArg {
    pub is_vararg: bool,
    pub name: Ident,
    pub ty: Ident,
}
