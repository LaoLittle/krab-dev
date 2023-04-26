use super::Parser;
use crate::errors::Error;
use crate::stream::{IntSuffix, Literal, Token};
use kotlin_ast::expr::{BinaryOp, ExprStmt, IntTy, UnaryOp};
use kotlin_span::Ident;

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> ExprStmt {
        let expr = self.parse_unary_expr();
        let expr = self.parse_binary_expr(expr, 0, Self::peek_token);

        expr
    }

    pub fn parse_lambda_expr(&mut self, label: Option<Ident>) -> ExprStmt {
        let body = self.parse_block();

        ExprStmt::lambda(body, label)
    }

    pub fn parse_unary_expr(&mut self) -> ExprStmt {
        match self.advance_token_skip_nl() {
            lit @ (Token::True | Token::False) => match lit {
                Token::True => ExprStmt::lit_bool(true),
                Token::False => ExprStmt::lit_bool(false),
                _ => unreachable!(),
            },
            Token::Literal(lit) => match lit {
                Literal::Integer { int, suffix } => {
                    let suffix = match suffix {
                        Some(IntSuffix::Long) => Some(IntTy::Long),
                        Some(IntSuffix::Unsigned) => Some(IntTy::Unsigned),
                        Some(IntSuffix::Unknown(sp)) => {
                            self.errors.push(Error::UnknownSuffix { suffix: sp });
                            None
                        }
                        None => None,
                    };

                    ExprStmt::lit_integer(int, suffix)
                }
            },
            Token::OpenParen => {
                let e = self.parse_unary_expr();
                let e = self.parse_binary_expr(e, 0, Self::peek_token_skip_nl);
                self.expect(Token::CloseParen);
                ExprStmt::paren(e)
            }
            Token::Ident => {
                let id = self.last_ident();
                match self.peek_token() {
                    Token::At => {
                        self.bump();
                        self.expect_skip_nl(Token::OpenBrace);
                        let expr = self.parse_lambda_expr(Some(id));
                        self.expect_skip_nl(Token::CloseBrace);
                        expr
                    }
                    _ => ExprStmt::Ident(id),
                }
            }
            unop @ (Token::Plus | Token::Minus | Token::Inc | Token::Dec | Token::Not) => {
                let unop = match unop {
                    Token::Plus => UnaryOp::Positive,
                    Token::Minus => UnaryOp::Negative,
                    Token::Inc => UnaryOp::PreInc,
                    Token::Dec => UnaryOp::PreDec,
                    Token::Not => UnaryOp::Not,
                    _ => unreachable!(),
                };
                let e = self.parse_expr().peel_paren();

                match (&unop, &e) {
                    (UnaryOp::PreInc | UnaryOp::PreDec, ExprStmt::Ident(_)) => {}
                    (UnaryOp::PreInc | UnaryOp::PreDec, _) => return ExprStmt::bad(),
                    _ => {}
                }

                ExprStmt::unary(unop, e)
            }
            Token::OpenBrace => {
                let expr = self.parse_lambda_expr(None);
                self.expect_skip_nl(Token::CloseBrace);
                expr
            }
            Token::Return => {
                let mut at = None;
                if let Token::At = self.peek_token() {
                    self.bump();
                    self.expect(Token::Ident);
                    at = Some(self.last_ident());
                }

                let mut expr = None;
                if !matches!(
                    self.peek_token(),
                    Token::NewLine
                        | Token::Semi
                        | Token::CloseParen
                        | Token::CloseBrace
                        | Token::CloseBracket
                        | Token::Elvis // return ?: value
                        | Token::Eof
                ) {
                    expr = Some(self.parse_expr());
                }

                ExprStmt::r#return(expr, at)
            }
            Token::If => self.parse_if_expr(),
            Token::Null => ExprStmt::null(),
            tk => {
                self.lookahead = Some(tk);

                ExprStmt::bad()
            }
        }
    }

    /// expr -> expr bin_op expr
    /// expr -> expr bin_expr
    /// bin_expr -> bin_op expr
    pub fn parse_binary_expr(
        &mut self,
        mut lhs: ExprStmt,
        lprec: u32,
        peek: fn(&mut Self) -> Token,
    ) -> ExprStmt {
        loop {
            match peek(self) {
                binop @ (Token::Plus
                | Token::Minus
                | Token::Mul
                | Token::Div
                | Token::Rem
                | Token::And
                | Token::Or
                | Token::Eq
                | Token::ExactEq
                | Token::Greater
                | Token::GreaterEq
                | Token::Less
                | Token::LessEq
                | Token::NotEq
                | Token::NotExactEq
                | Token::Elvis
                | Token::DotDot
                | Token::Ident) => {
                    let binop = match binop {
                        Token::Plus => BinaryOp::Add,
                        Token::Minus => BinaryOp::Sub,
                        Token::Mul => BinaryOp::Mul,
                        Token::Div => BinaryOp::Div,
                        Token::Rem => BinaryOp::Rem,
                        Token::And => BinaryOp::And,
                        Token::Or => BinaryOp::Or,
                        Token::Eq => BinaryOp::Eq,
                        Token::ExactEq => BinaryOp::ExEq,
                        Token::Greater => BinaryOp::Gt,
                        Token::GreaterEq => BinaryOp::Ge,
                        Token::Less => BinaryOp::Lt,
                        Token::LessEq => BinaryOp::Le,
                        Token::NotEq => BinaryOp::Ne,
                        Token::NotExactEq => BinaryOp::ExNe,
                        Token::Elvis => BinaryOp::Elvis,
                        Token::DotDot => BinaryOp::Range,
                        Token::Ident => BinaryOp::Infix,
                        _ => unreachable!(),
                    };

                    let rprec = binop.precedence();

                    // 0 a 60 + 61 b 70 * 71 c 60 + 61 dd
                    if lprec < rprec {
                        self.bump();
                        let rhs = self.parse_unary_expr();
                        let rhs = self.parse_binary_expr(rhs, rprec + 1, peek);

                        lhs = ExprStmt::binary(binop, lhs, rhs);
                    } else {
                        break;
                    }
                }
                Token::Semi | Token::Eof => {
                    self.bump(); // eat ';' then break
                    break;
                }
                tk => {
                    if let Token::NewLine = tk {
                        self.bump();
                        if let Token::Elvis = peek(self) {
                            const ELVIS_PRECEDENCE: u32 = 50;
                            if lprec < ELVIS_PRECEDENCE {
                                self.bump();
                                let rhs = self.parse_unary_expr();
                                let rhs = self.parse_binary_expr(rhs, ELVIS_PRECEDENCE + 1, peek);

                                lhs = ExprStmt::binary(BinaryOp::Elvis, lhs, rhs);
                                continue;
                            } else {
                                break;
                            }
                        } else {
                            self.keep_nl = true;
                        }
                    }

                    match self.parse_assoc_expr(lhs) {
                        Ok(expr) => lhs = expr,
                        Err(expr) => return expr,
                    }
                }
            }
        }

        lhs
    }

    pub fn parse_assoc_expr(&mut self, expr: ExprStmt) -> Result<ExprStmt, ExprStmt> {
        match self.peek_token() {
            // function call
            // expr\n(expr)
            // -x> expr(expr)
            // --> expr; (expr)
            Token::OpenParen => {
                self.bump();
                let args = self.parse_call_args();
                self.expect_skip_nl(Token::CloseParen);

                return Ok(ExprStmt::call(expr, args));
            }
            _ => {}
        }

        let mut nl = false;
        Ok(loop {
            break match self.advance_token() {
                Token::OpenBracket => {
                    let idx = self.parse_expr();
                    self.expect_skip_nl(Token::CloseBracket);

                    ExprStmt::index(expr, idx)
                }
                // method call or property get
                Token::Dot => {
                    self.expect_skip_nl(Token::Ident);
                    let id = self.last_ident();

                    ExprStmt::selector(expr, id)
                }
                Token::Inc => ExprStmt::unary(UnaryOp::PostInc, expr),
                Token::Dec => ExprStmt::unary(UnaryOp::PostDec, expr),
                Token::NewLine => {
                    nl = true;
                    continue;
                }
                tk => {
                    self.keep_nl = nl;
                    self.lookahead = Some(tk);
                    return Err(expr);
                }
            };
        })
    }

    pub fn parse_if_expr(&mut self) -> ExprStmt {
        self.expect_skip_nl(Token::OpenParen);
        let cond = self.parse_expr();
        self.expect_skip_nl(Token::CloseParen);
        let then = self.parse_block_even_single_expr();

        let r#else = if let Token::Else = self.peek_token_skip_nl() {
            self.bump();
            Some(self.parse_block_even_single_expr())
        } else {
            None
        };

        ExprStmt::r#if(cond, then, r#else)
    }

    fn parse_call_args(&mut self) -> Vec<ExprStmt> {
        let mut args = Vec::new();

        if let Token::CloseParen = self.peek_token_skip_nl() {
            return args;
        };

        loop {
            let expr = self.parse_expr();
            args.push(expr);

            match self.peek_token_skip_nl() {
                Token::Comma => {
                    self.bump();
                }
                Token::CloseParen => break,
                _ => break,
            }
        }

        args
    }
}
