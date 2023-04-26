use std::collections::HashMap;
use kotlin_parse::Parser;
use kotlin_span::symbol::Symbol;
use crate::hir::Variable;

pub struct Visitor<'a> {
    parser: Parser<'a>,
    var_idx: usize,
    var_table: HashMap<Symbol, Variable>,
}

impl<'a> Visitor<'a> {
    pub fn new(parser: Parser<'a>) -> Self {
        Self {
            parser,
            var_idx: 0,
            var_table: HashMap::new(),
        }
    }

    pub fn new_var() {

    }
}