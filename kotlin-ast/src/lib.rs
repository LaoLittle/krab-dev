use kotlin_span::symbol::Symbol;
use kotlin_span::Span;

pub mod block;
pub mod decl;
pub mod expr;
pub mod file;
pub mod stmt;

#[derive(Copy, Clone, Debug)]
pub struct Ident {
    symbol: Symbol,
    span: Span,
}

impl Ident {
    #[inline]
    pub const fn new(symbol: Symbol, span: Span) -> Self {
        Self { symbol, span }
    }

    pub const fn symbol(self) -> Symbol {
        self.symbol
    }

    #[inline]
    pub const fn span(self) -> Span {
        self.span
    }
}
