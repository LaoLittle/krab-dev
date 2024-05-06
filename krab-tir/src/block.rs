use crate::stmt::Stmt;

#[derive(Debug, Copy, Clone)]
pub struct Block<'tir> {
    pub stmts: &'tir [Stmt<'tir>],
}
