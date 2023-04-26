use crate::errors::Error;
use crate::stream::{Token, TokenStream};
use kotlin_span::symbol::Symbol;
use kotlin_span::Ident;
use kotlin_span::Span;

mod block;
mod decl;
mod errors;
mod expr;
mod file;
mod stmt;
mod stream;

pub struct Parser<'a> {
    stream: TokenStream<'a>,
    lookahead: Option<Token>,
    keep_nl: bool,
    errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    #[inline]
    pub fn new(input: &'a str) -> Self {
        Self {
            stream: TokenStream::new(input),
            lookahead: None,
            keep_nl: false,
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
        if self.keep_nl {
            self.keep_nl = false;
            return Token::NewLine;
        }

        self.lookahead
            .take()
            .unwrap_or_else(|| self.stream.advance_token())
    }

    pub fn advance_token_skip_nl(&mut self) -> Token {
        self.keep_nl = false;
        let mut tk = self.advance_token(); // get lookahead
        loop {
            match tk {
                Token::NewLine => {
                    tk = self.stream.advance_token();
                    continue;
                }
                _ => return tk,
            }
        }
    }

    pub fn peek_token(&mut self) -> Token {
        if self.keep_nl {
            return Token::NewLine;
        }

        if let Some(tk) = self.lookahead.clone() {
            return tk;
        }

        let tk = self.stream.advance_token();
        self.lookahead = Some(tk.clone());

        tk
    }

    pub fn peek_token_skip_nl(&mut self) -> Token {
        self.keep_nl = false;

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

    pub fn last_ident(&self) -> Ident {
        let span = self.last_span();
        let s = &self.source()[span.range()];
        let sym = Symbol::intern(s);

        Ident::new(sym, span)
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

    #[inline]
    pub fn keep_nl(&mut self) {
        self.keep_nl = true;
    }

    pub fn get_line(&self, pos: usize) -> &str {
        const NL: char = '\u{000A}';

        let source = self.source();
        assert!(pos < source.len());

        let (back, front) = source.split_at(pos);
        let b = back.rfind(NL).map(|i| i + 1).unwrap_or(0);

        let f = front.find(NL).map(|i| i + pos).unwrap_or(source.len());

        &source[b..f]
    }
}

#[cfg(test)]
mod tests {

    use crate::{Parser, TokenStream};
    use kotlin_ast::decl::{DeclStmt, FunDecl, PackageDecl};
    use kotlin_ast::stmt::Stmt;
    use kotlin_span::with_global_session_init;
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
        with_global_session_init(|| {
            let prev = Instant::now();
            let mut parser = Parser::new(
                r#"
        dad(min.a().c())
        "#,
            );
            let expr = parser.parse_expr();
            let now = Instant::now();
            println!("Parsed: {:#?}\ncost {}μs", expr, now.sub(prev).as_micros());

            println!("------ errors ------");
            for error in parser.errors() {
                println!("{}", error);
            }
            println!("------  end   ------");
        })
    }

    #[test]
    fn pkg() {
        let mut parser = Parser::new("a.b.cddd.dad");
        let pkg = parser.parse_package_decl();
        println!("{:?}", pkg);
        if let PackageDecl::Valid(span) = pkg {
            println!("{:?}", &parser.source()[span.range()]);
        }
    }

    #[test]
    fn import() {
        with_global_session_init(|| {
            let mut parser = Parser::new("import Class\nimport a.b.c.Class");
            let stmt = parser.parse_stmt();
            println!("{:?}", stmt);
        })
    }

    #[test]
    fn stmt() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
                package a
        "#,
            );
            let stmt = parser.parse_stmt();

            if let Stmt::Decl(DeclStmt::Package(PackageDecl::Valid(s))) = &stmt {
                println!("{:?}", &parser.source()[s.range()]);
            }
            println!("{:#?}", stmt);

            println!("------ errors ------");
            for error in parser.errors() {
                println!("{}", error);
            }
            println!("------  end   ------");
        });
    }

    #[test]
    fn block() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
        a@{
        val a: Int = eae;
        bab.b()
        ddd()
        }
        "#,
            );

            println!("{:#?}", parser.parse_expr());
        });
    }

    #[test]
    fn fun() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
            fun
            bbbaaa  (
            a:
            String
            ): A {
            fun a() {
            aba ?: c

            ?: d
            +b
            }

            val bab = dd @ { val bb = eae; ca }

            val a = b
            }
            "#,
            );

            let stmt = parser.parse_stmt();
            if let Stmt::Decl(DeclStmt::Fun(FunDecl {
                name,
                args,
                body,
                ret_type,
            })) = stmt
            {
                println!("function name is {:?}", name.symbol().as_str());
                println!("args: {:#?}", args);
                println!("body: {:#?}", body);
                println!("ret type: {:#?}", ret_type);

                assert_eq!(parser.errors().len(), 0);
            } else {
                panic!("not a fun");
            }
        });
    }

    #[test]
    fn lit() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
                true
                "#,
            );
            let expr = parser.parse_expr();
            println!("{:?}", expr);
        });
    }

    #[test]
    fn get_ln() {
        let parser = Parser::new("abc\ncdbb\ndada");

        println!("{:?}", parser.get_line(12));
    }

    #[test]
    fn if_expr() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
        if ((true && bbb))
{
        (false || a)
        } else true
        "#,
            );

            for _ in 0..1 {
                println!("{:#?}", parser.parse_expr());
            }

            for error in parser.errors() {
                println!("{error}");
            }
        });
    }

    #[test]
    fn while_stmt() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
                while (true) {}
                "#,
            );
            println!("{:?}", parser.parse_stmt());
        });
    }

    #[test]
    fn for_stmt() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
                for (i in a..b) {}
                "#,
            );
            println!("{:?}", parser.parse_stmt());
        });
    }

    #[test]
    fn integer() {
        with_global_session_init(|| {
            let mut parser = Parser::new(
                r#"
                23+123 * 456L
                "#,
            );
            println!("{:?}", parser.parse_stmt());
        });
    }
}
