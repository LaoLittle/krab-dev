use crate::stream::Token;
use crate::Parser;
use kotlin_ast::decl::{DeclStmt, ImportDecl, PackageDecl};
use kotlin_ast::expr::ExprStmt;
use kotlin_ast::stmt::{AssignKind, AssignStmt, Stmt};
use kotlin_ast::Ident;
use kotlin_span::Span;

impl<'a> Parser<'a> {
    pub fn parse_stmt_list(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        while match self.peek_token_skip_nl() {
            Token::Eof | Token::CloseBrace => false,
            _ => true,
        } {
            stmts.push(self.parse_stmt());
        }

        stmts
    }

    pub fn parse_stmt(&mut self) -> Stmt {
        match self.advance_token_skip_nl() {
            Token::Package => Stmt::Decl(DeclStmt::Package(self.parse_package_decl())),
            Token::Import => Stmt::Decl(DeclStmt::Import(self.parse_import_decl())),
            Token::Fun => Stmt::Decl(DeclStmt::Fun(self.parse_fun_decl())),
            Token::Val => Stmt::Assign(self.parse_assign_stmt(false)),
            Token::Var => Stmt::Assign(self.parse_assign_stmt(true)),
            Token::Semi | Token::Eof => Stmt::Empty,
            tk => {
                self.lookahead = Some(tk);
                Stmt::Expr(self.parse_expr())
            }
        }
    }

    pub fn parse_assign_stmt(&mut self, mutable: bool) -> AssignStmt {
        self.expect_skip_nl(Token::Ident);
        let name = self.last_ident();

        let ty = match self.peek_token_skip_nl() {
            Token::Colon => {
                self.bump();
                self.expect_skip_nl(Token::Ident);
                Some(self.last_ident())
            }
            _ => None,
        };

        let mut skipped = false;
        let kind = loop {
            match self.advance_token() {
                Token::NewLine => skipped = true,
                Token::Assign => break AssignKind::Init(self.parse_expr()),
                tk => {
                    self.lookahead = Some(tk);
                    if !skipped {
                        self.expect(Token::Assign);
                    }

                    break AssignKind::Decl;
                }
            }
        };

        AssignStmt {
            mutable,
            name,
            ty,
            kind,
        }
    }
}
