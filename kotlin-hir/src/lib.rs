use kotlin_span::Ident;
use std::rc::Rc;

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
pub struct Variable(Rc<VariableInner>);

impl Variable {
    pub fn new() {}
}

#[derive(Debug)]
struct VariableInner {
    assignable: bool,
    name: Ident,
}

impl Variable {}

#[derive(Debug)]
pub struct Stmt {}

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
