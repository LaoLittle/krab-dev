use super::Parser;
use crate::stream::Token;
use kotlin_ast::expr::{BinaryOp, ExprStmt, UnaryOp};
use kotlin_ast::Ident;
use std::fmt::Write;

impl<'a> Parser<'a> {
    /// unary_expr -> '(' expr ')' | unary_op expr |
    pub fn parse_expr(&mut self) -> ExprStmt {
        let expr = self.parse_unary_expr();
        let expr = self.parse_binary_expr(expr, 0, Self::peek_token);

        expr
    }

    pub fn parse_unary_expr(&mut self) -> ExprStmt {
        match self.advance_token_skip_nl() {
            Token::OpenParen => {
                let e = self.parse_unary_expr();
                let e = self.parse_binary_expr(e, 0, Self::peek_token_skip_nl);
                self.expect(Token::CloseParen).unwrap();
                ExprStmt::paren(e)
            }
            Token::Ident => ExprStmt::Ident(Ident::new_with_end(
                self.stream.prev_pos(),
                self.stream.pos(),
            )),
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
            Token::Return => {
                let mut at = None;
                if let Token::At = self.peek_token() {
                    self.bump();
                    self.expect(Token::Ident).unwrap();
                    at = Some(self.last_ident());
                }

                let mut expr = None;
                if !matches!(self.peek_token(),
                    Token::NewLine
                    | Token::Semi
                    | Token::CloseParen
                    | Token::CloseBrace
                    | Token::CloseBracket
                    | Token::Eof
                )
                {
                    expr = Some(self.parse_expr());
                }

                ExprStmt::r#return(expr, at)
            }
            Token::Null => ExprStmt::null(),
            tk => {
                self.lookahead = Some(tk);
                ExprStmt::bad()
            },
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
                    } else { break; }
                }
                Token::Semi | Token::Eof => break,
                _ => match self.parse_assoc_expr(lhs) {
                    Ok(expr) => lhs = expr,
                    Err(expr) => return expr,
                },
            }
        }

        lhs
    }

    pub fn parse_assoc_expr(&mut self, expr: ExprStmt) -> Result<ExprStmt, ExprStmt> {
        Ok(match self.advance_token_skip_nl() {
            // function call
            Token::OpenParen => {
                let args = self.parse_call_args();
                self.expect_skip_nl(Token::CloseParen).unwrap();

                ExprStmt::call(expr, args)
            }
            Token::OpenBracket => {
                let idx = self.parse_expr();
                self.expect_skip_nl(Token::CloseBracket).unwrap();

                ExprStmt::index(expr, idx)
            }
            // method call or property get
            Token::Dot => {
                self.expect_skip_nl(Token::Ident).unwrap();
                let id = self.last_ident();

                ExprStmt::selector(expr, id)
            }
            Token::Inc => ExprStmt::unary(UnaryOp::PostInc, expr),
            Token::Dec => ExprStmt::unary(UnaryOp::PostDec, expr),
            tk => {
                self.lookahead = Some(tk);
                return Err(expr);
            }
        })
    }

    pub fn parse_call_args(&mut self) -> Vec<ExprStmt> {
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

    pub fn parse_decl(&mut self) -> Option<kotlin_ast::decl::Decl> {
        let tk = self.advance_token_skip_nl();
        match tk {
            Token::Fun => {
                println!("parse function");
                let name = self.advance_token_skip_nl();
                let id = Ident::new_with_end(self.stream.prev_pos(), self.stream.pos());
                let si = &self.source()[id.span().range()];
                println!("name={si}");
                if !matches!(name, Token::Ident) {
                    let s = self
                        .source()
                        .get(
                            (self.stream.prev_pos().saturating_sub(4))
                                ..(self.stream.pos().saturating_add(4)),
                        )
                        .unwrap_or(si);
                    println!("{s}");
                    let mut s =
                        String::with_capacity(self.stream.pos() - self.stream.prev_pos() + 4);
                    for _ in 0..4 {
                        s.push(' ');
                    }
                    write!(s, "^ expected identifier, found {:?}", si).unwrap();
                    println!("{s}");
                    return None;
                }

                None
            }
            _ => None,
        }
    }
}
