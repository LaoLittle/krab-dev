use kotlin_ast::expr::*;
use kotlin_span::symbol::Symbol;
use rustc_hash::FxHashMap;

pub enum Type {
    Boolean,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Char,
}

pub trait Codegen {
    type Var;

    fn alloca_stack_var() -> Self::Var;
    fn create_var() -> Self::Var;
    fn store_i1(var: &mut Self::Var, val: bool);
    fn read_i1(src: &Self::Var, dst: &mut Self::Var);

    fn var_type(&self, var: &Self::Var) -> Type;
}

pub struct Visitor<C: Codegen> {
    codegen: C,
    var_maps: Vec<FxHashMap<Symbol, C::Var>>,
}

impl<C: Codegen> Visitor<C> {
    pub fn new(c: C) -> Self {
        Self {
            codegen: c,
            var_maps: Vec::with_capacity(10),
        }
    }

    pub fn visit_expr(&mut self, expr: ExprStmt) -> C::Var {
        unimplemented!()
    }

    pub fn visit_binary_expr(&mut self, binary: BinaryExpr) {
        match binary {
            BinaryExpr {
                op: BinaryOp::Add,
                lhs,
                rhs,
            } => {
                let lhs = self.visit_expr(*lhs);
                let rhs = self.visit_expr(*rhs);
            }
            _ => unimplemented!(),
        }
    }

    pub fn find_var(&self, symbol: Symbol) -> Option<&C::Var> {
        for map in self.var_maps.iter().rev() {
            if let Some(var) = map.get(&symbol) {
                return Some(var);
            }
        }

        None
    }

    pub fn remove_var(&mut self, symbol: Symbol) -> Option<C::Var> {
        self.var_maps.last_mut().and_then(|map| map.remove(&symbol))
    }

    pub fn new_scope(&mut self) {
        self.var_maps.push(FxHashMap::default());
    }

    pub fn end_scope(&mut self) {
        self.var_maps.pop();
    }

    fn cg(&mut self) -> &mut C {
        &mut self.codegen
    }
}
