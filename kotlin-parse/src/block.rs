use crate::stream::Token;
use crate::Parser;
use kotlin_ast::block::Block;
use kotlin_ast::stmt::Stmt;
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

    pub fn parse_block_even_single_expr(&mut self) -> Block {
        if let Token::OpenBrace = self.peek_token_skip_nl() {
            self.bump();
            let b = self.parse_block();
            self.expect_skip_nl(Token::CloseBrace);
            b
        } else {
            self.parse_single_expr_block()
        }
    }

    pub fn parse_single_expr_block(&mut self) -> Block {
        let start = self.pos();
        let expr = self.parse_expr();
        let end = self.pos();

        Block {
            stmts: vec![Stmt::Expr(expr)],
            span: Span::new_with_end(start, end),
        }
    }
}
