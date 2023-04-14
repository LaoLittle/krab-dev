use crate::errors::Error;
use crate::stream::{Token, TokenStream};
use kotlin_ast::{Ident, Span};

mod errors;
mod expr;
mod stmt;
mod stream;

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    lookahead: Option<Token>,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: TokenStream::new(input),
            lookahead: None,
            errors: vec![],
        }
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn take_errors(&mut self) -> Vec<Error> {
        std::mem::take(&mut self.errors)
    }

    #[inline]
    pub fn advance_token(&mut self) -> Token {
        self.lookahead
            .take()
            .unwrap_or_else(|| self.stream.advance_token())
    }

    pub fn advance_token_skip_nl(&mut self) -> Token {
        let mut tk = self.advance_token();
        loop {
            match tk {
                Token::NewLine => {
                    tk = self.stream.advance_token();
                    continue;
                }
                or => return or,
            }
        }
    }

    pub fn peek_token(&mut self) -> Token {
        if let Some(tk) = self.lookahead.clone() {
            return tk;
        }

        let tk = self.stream.advance_token();
        self.lookahead = Some(tk.clone());

        tk
    }

    pub fn peek_token_skip_nl(&mut self) -> Token {
        match self.lookahead.clone() {
            Some(Token::NewLine) | None => {}
            Some(or) => return or,
        }

        let tk = self.advance_token_skip_nl();
        self.lookahead = Some(tk.clone());

        tk
    }

    pub fn expect(&mut self, target: Token) {
        let tk = self.advance_token();
        self._expect_token(tk, target);
    }

    pub fn expect_skip_nl(&mut self, target: Token) {
        let tk = self.advance_token_skip_nl();
        self._expect_token(tk, target);
    }

    pub fn _expect_token(&mut self, token: Token, target: Token) {
        if token != target {
            self.lookahead = Some(token.clone());
            self.errors.push(Error::UnexpectedToken {
                expect: target,
                actual: token,
                span: self.last_span(),
            });
        }
    }

    pub const fn last_span(&self) -> Span {
        Span::new_with_end(self.stream.prev_pos(), self.stream.pos())
    }

    pub const fn last_ident(&self) -> Ident {
        Ident::new_with_end(self.stream.prev_pos(), self.stream.pos())
    }

    #[inline]
    pub const fn source(&self) -> &str {
        self.stream.source()
    }

    #[inline]
    pub fn bump(&mut self) {
        self.advance_token();
    }

    pub fn bump_semi(&mut self) {
        if let Token::Semi = self.peek_token() {
            self.bump();
        }
    }

    #[inline]
    pub const fn prev_pos(&self) -> usize {
        self.stream.prev_pos()
    }

    #[inline]
    pub const fn pos(&self) -> usize {
        self.stream.pos()
    }
}

#[cfg(test)]
mod tests {
    use crate::stmt::PackageStmt;
    use crate::{Parser, TokenStream};
    use std::ops::Sub;
    use std::time::Instant;

    #[test]
    fn token() {
        let input = r#"
        return@a 1
    "#;

        for token in TokenStream::new(input) {
            println!("{:?}", token);
        }
    }

    #[test]
    fn parse() {
        let prev = Instant::now();
        let mut parser = Parser::new(
            r#"
        a ?: return ?: ee
        "#,
        );
        let decl = parser.parse_expr();
        let now = Instant::now();
        println!("Parsed: {:#?}\ncost {}μs", decl, now.sub(prev).as_micros());

        println!("------ errors ------");
        for error in parser.errors() {
            println!("{}", error);
        }
        println!("------  end   ------");
    }

    #[test]
    fn pkg() {
        let mut parser = Parser::new("package a.b.cddd.");
        let pkg = parser.parse_package_stmt();
        println!("{:?}", pkg);
        if let PackageStmt::Valid(span) = pkg {
            println!("{:?}", &parser.source()[span.range()]);
        }
    }
}
