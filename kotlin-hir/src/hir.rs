use kotlin_span::Ident;
use std::rc::Rc;

#[derive(Debug)]
pub struct ReferType {
    /// type suspect
    sus: bool,
    /// type
    ty: Type,
}

#[derive(Debug)]
pub enum Type {
    Unit,
    Boolean,
    Int(IntTy),
}

#[derive(Debug)]
pub enum IntTy {
    Int8,
    Int16,
    Int32,
    Int64,
}

#[derive(Debug)]
pub struct Variable {
    assignable: bool,
    name: Ident,
    ty: Option<ReferType>,
    initialized: bool,
    idx: usize,
}

impl Variable {

}

#[derive(Debug)]
pub struct Stmt {}

pub enum StmtKind {
    Local(),
}



#[derive(Debug)]
pub struct Expr {
    ty: Type,
    kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    Lit(LitExpr),
    Var(Variable),
}

#[derive(Debug)]
pub enum LitExpr {
    Boolean(bool),
    Integer(u128),
}

impl Expr {
    pub fn modify_type(&mut self, prefer: Option<Type>) {

    }
}