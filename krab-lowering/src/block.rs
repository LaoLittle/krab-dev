use kotlin_ast::block::Block as AstBlock;
use krab_tir::block::Block;

use crate::LoweringContext;

impl<'tir> LoweringContext<'tir> {
    pub fn lowering_block(&mut self, block: &'tir AstBlock) -> Block<'tir> {
        Block {
            stmts: self.lowering_stmt_list(&block.stmts),
        }
    }
}
