use crate::common::IdxVec;
use crate::middle::ir;

#[derive(Debug)]
pub(super) struct IrMap<'ir> {
    defs: IdxVec<ir::DefIdx, BlockMap<'ir>>,
}

#[derive(Debug)]
pub(super) struct BlockMap<'ir> {
    blocks: IdxVec<ir::BlockIdx, &'ir ir::Block>,
}

impl<'ir> IrMap<'ir> {
    pub(super) fn new(ir: &'ir ir::Ir) -> IrMap<'ir> {
        let mut defs: IdxVec<ir::DefIdx, _> = IdxVec::new();
        for def in ir.defs.values() {
            defs.push(BlockMap::new(&def));
        }
        IrMap { defs }
    }

    #[inline]
    pub(super) fn blocks(&self, def: ir::DefIdx) -> &BlockMap<'ir> {
        &self.defs[def]
    }
}

impl<'ir> BlockMap<'ir> {
    fn new(def: &'ir ir::Def) -> BlockMap<'ir> {
        let mut block_map = BlockMap {
            blocks: IdxVec::new(),
        };
        block_map.register_block(&def.entry.body);
        block_map
    }

    fn register_block(&mut self, block: &'ir ir::Block) {
        let idx = self.blocks.push(block);
        if let ir::Terminator::Match { arms, .. } = &block.terminator {
            for arm in arms.iter() {
                self.register_block(&arm.target);
            }
        }
        debug_assert_eq!(idx, block.block_idx);
    }

    #[inline]
    pub(super) fn lookup(&self, block: ir::BlockIdx) -> &'ir ir::Block {
        &self.blocks[block]
    }

    #[inline]
    pub(super) fn size(&self) -> usize {
        self.blocks.len()
    }
}
