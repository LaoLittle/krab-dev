use kotlin_span::Ident;

#[derive(Debug)]
pub struct ReferType {
    /// suspect
    pub sus: bool,
    /// type
    pub ty: Type,
}

#[derive(Debug)]
pub enum Type {
    Unit,
    Boolean,
    Int(IntTy),
    Class(),
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
    pub assignable: bool,
    pub name: Ident,
    pub ty: Option<ReferType>,
    pub initialized: bool,
}

impl Variable {}

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
