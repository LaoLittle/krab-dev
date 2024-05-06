use kotlin_span::symbol::Symbol;
use kotlin_span::Ident;
use krab_tir::expr::Expr;
use krab_tir::stmt::Stmt;
use krab_tir::ty::Type;
use smallvec::SmallVec;
use std::collections::HashMap;
use typed_arena::Arena;

mod block;
pub mod expr;
pub mod stmt;

type TyMap = HashMap<Symbol, (Type, bool)>;

#[derive(Default)]
pub struct TirCtx<'tir> {
    expr_arena: Arena<Expr<'tir>>,
    stmt_arena: Arena<Stmt<'tir>>,
    type_arena: Arena<Type>,
    type_id_alloc: Arena<(Ident, Type)>,
}

pub struct LoweringContext<'tir> {
    scopes: SmallVec<TyMap, 8>,

    expr_alloc: &'tir Arena<Expr<'tir>>,
    stmt_alloc: &'tir Arena<Stmt<'tir>>,
    type_alloc: &'tir Arena<Type>,
    type_id_alloc: &'tir Arena<(Ident, Type)>,

    curr_ret: Option<Type>,
}

impl<'tir> LoweringContext<'tir> {
    pub fn new(ctx: &'tir TirCtx<'tir>) -> Self {
        Self {
            scopes: SmallVec::new(),
            expr_alloc: &ctx.expr_arena,
            stmt_alloc: &ctx.stmt_arena,
            type_alloc: &ctx.type_arena,
            type_id_alloc: &ctx.type_id_alloc,
            curr_ret: None,
        }
    }

    fn find_ty(&self, id: Ident) -> Option<Type> {
        let sym = id.symbol();

        for map in self.scopes.iter().rev() {
            let Some(ty) = map.get(&sym) else {
                continue;
            };

            return Some(ty.0.clone());
        }

        None
    }

    fn get_ty_mut(&mut self, id: Ident) -> &mut Type {
        let sym = id.symbol();

        for map in self.scopes.iter_mut().rev() {
            let Some(ty) = map.get_mut(&sym) else {
                continue;
            };

            return &mut ty.0;
        }

        panic!("unable to find ty of {id:?}")
    }

    fn current_scope(&mut self) -> &mut TyMap {
        self.scopes.last_mut().unwrap()
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(TyMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}
