use crate::stream::{Token, TokenStream};
use kotlin_ast::{Ident, Span};

mod expr;
mod stream;

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    lookahead: Option<Token>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: TokenStream::new(input),
            lookahead: None,
        }
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

    pub fn expect(&mut self, target: Token) -> Result<(), Token> {
        let tk = self.advance_token();
        if tk == target {
            Ok(())
        } else {
            self.lookahead = Some(tk.clone());
            Err(tk)
        }
    }

    pub fn expect_skip_nl(&mut self, target: Token) -> Result<(), Token> {
        let tk = self.advance_token_skip_nl();
        if tk == target {
            Ok(())
        } else {
            self.lookahead = Some(tk.clone());
            Err(tk)
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
}

#[cfg(test)]
mod tests {
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
        let mut parser = Parser::new(r#"

        "#);
        let decl = parser.parse_expr();
        let now = Instant::now();
        println!("Parsed: {:#?}\ncost {}μs", decl, now.sub(prev).as_micros());
    }
}
