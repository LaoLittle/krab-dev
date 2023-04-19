use crate::block::Block;
use crate::Ident;
use kotlin_span::Span;
use crate::expr::ExprStmt;

#[derive(Debug)]
pub enum DeclStmt {
    Variable(VariableDecl),
    Package(PackageDecl),
    Import(ImportDecl),
    Fun(FunDecl),
}

#[derive(Debug)]
pub struct VariableDecl {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Option<Ident>,
    pub kind: VarKind,
}

#[derive(Debug)]
pub enum VarKind {
    Decl,
    Init(ExprStmt),
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
    pub ret_type: Option<Ident>,
}

#[derive(Debug)]
pub struct FunArg {
    pub is_vararg: bool,
    pub name: Ident,
    pub ty: Ident,
}
