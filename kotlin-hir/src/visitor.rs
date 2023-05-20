use crate::hir::Variable;
use kotlin_parse::Parser;
use kotlin_span::symbol::Symbol;
use std::collections::HashMap;

pub struct Visitor<'a, T = ()> {
    parser: Parser<'a>,
    var_idx: usize,
    var_table: HashMap<Symbol, (Variable, T)>,
    var_data: fn() -> T,
}

impl<'a, T> Visitor<'a, T> {
    pub fn new(parser: Parser<'a>, var_data: fn() -> T) -> Self {
        Self {
            parser,
            var_idx: 0,
            var_table: HashMap::new(),
            var_data,
        }
    }

    pub fn new_var(&mut self, var: Variable) {
        let data = (self.var_data)();
        self.var_table.insert(var.name.symbol(), (var, data));
    }
}
