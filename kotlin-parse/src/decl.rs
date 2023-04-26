use crate::stream::Token;
use crate::Parser;
use kotlin_ast::decl::{FunArg, FunDecl, ImportDecl, PackageDecl};
use kotlin_span::Span;

impl<'a> Parser<'a> {
    pub fn parse_package_decl(&mut self) -> PackageDecl {
        self.peek_token(); // skip whitespaces

        let prev = self.prev_pos();

        match self.check_valid_package_name() {
            Ok(pos) => PackageDecl::Valid(Span::new_with_end(prev, pos)),
            Err(pos) => PackageDecl::InvalidName(pos),
        }
    }

    pub fn parse_import_decl(&mut self) -> ImportDecl {
        self.peek_token();

        let prev = self.prev_pos();

        match self.check_valid_package_name() {
            Ok(_) => {
                let id_start = self.prev_pos();

                ImportDecl {
                    package: if id_start == prev {
                        None
                    } else {
                        Some(Span::new_with_end(prev, id_start - 1))
                    },
                    class: self.last_ident(),
                }
            }
            Err(pos) => {
                panic!("error on pos={pos}");
            }
        }
    }

    fn check_valid_package_name(&mut self) -> Result<usize, usize> {
        fn invalid(parser: &mut Parser) -> Result<usize, usize> {
            Err(parser.prev_pos())
        }

        loop {
            self.expect(Token::Ident);
            match self.peek_token() {
                Token::NewLine | Token::Semi | Token::Eof => break,
                Token::Dot => {}
                _ => return invalid(self),
            }
            self.expect(Token::Dot);
            match self.peek_token() {
                Token::Ident => {}
                _ => return invalid(self),
            }
        }

        Ok(self.pos())
    }

    pub fn parse_fun_decl(&mut self) -> FunDecl {
        self.expect_skip_nl(Token::Ident);
        let name = self.last_ident();
        self.expect_skip_nl(Token::OpenParen);
        let args = self.parse_arg_list();
        self.expect_skip_nl(Token::CloseParen);
        let mut ret = None;
        if let Token::Colon = self.peek_token_skip_nl() {
            self.bump();
            self.expect_skip_nl(Token::Ident);
            ret = Some(self.last_ident());
        }
        self.expect_skip_nl(Token::OpenBrace);
        let body = self.parse_block();
        self.expect_skip_nl(Token::CloseBrace);

        FunDecl {
            name,
            args,
            body,
            ret_type: ret,
        }
    }

    fn parse_arg_list(&mut self) -> Vec<FunArg> {
        let mut v = vec![];
        let mut push_arg = |parser: &mut Parser, vararg: bool| -> Result<(), ()> {
            let name = parser.last_ident();
            parser.expect_skip_nl(Token::Colon);

            match parser.advance_token_skip_nl() {
                Token::Ident => {
                    let ty = parser.last_ident();
                    v.push(FunArg {
                        is_vararg: vararg,
                        name,
                        ty,
                    });
                }
                tk => {
                    parser.lookahead = Some(tk);
                    return Err(());
                }
            }
            if let Token::Colon = parser.peek_token_skip_nl() {
                parser.bump();
            }

            Ok(())
        };
        loop {
            match self.advance_token_skip_nl() {
                Token::Ident => {
                    if push_arg(self, false).is_err() {
                        break;
                    }
                }
                Token::Vararg => {
                    self.expect_skip_nl(Token::Ident);
                    if push_arg(self, true).is_err() {
                        break;
                    }
                }
                tk => {
                    self.lookahead = Some(tk);
                    break;
                }
            }
        }

        v
    }
}
