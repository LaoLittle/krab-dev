use crate::stream::Token;
use crate::Parser;
use kotlin_ast::expr::ExprStmt;
use kotlin_ast::Span;

#[derive(Debug)]
pub enum Stmt {
    Package(PackageStmt),
    Expr(ExprStmt),
}

#[derive(Debug)]
pub enum PackageStmt {
    Valid(Span),
    InvalidName(usize),
}

impl<'a> Parser<'a> {
    pub fn parse_package_stmt(&mut self) -> PackageStmt {
        self.expect_skip_nl(Token::Package);
        self.peek_token(); // skip whitespaces
        let prev = self.prev_pos();

        loop {
            self.expect(Token::Ident);
            match self.peek_token() {
                Token::NewLine | Token::Semi | Token::Eof => break,
                Token::Dot => {}
                _ => return PackageStmt::InvalidName(self.prev_pos()),
            }
            self.expect(Token::Dot);
            match self.peek_token() {
                Token::Ident => {}
                _ => return PackageStmt::InvalidName(self.prev_pos()),
            }
        }

        let pos = self.pos();

        PackageStmt::Valid(Span::new_with_end(prev, pos))
    }
}
