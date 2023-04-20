use crate::Parser;
use kotlin_ast::block::Block;
use kotlin_span::Span;

impl<'a> Parser<'a> {
    pub fn parse_block(&mut self) -> Block {
        let start = self.pos();
        let stmts = self.parse_stmt_list();
        let end = self.pos();

        Block {
            stmts,
            span: Span::new_with_end(start, end),
        }
    }
}
