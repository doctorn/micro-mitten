use crate::common::IdxVec;
use crate::middle::ir;

use std::collections::HashSet;

#[derive(Debug)]
pub(super) struct CallGraph {
    control_graphs: IdxVec<ir::DefIdx, ControlGraph>,
    callers: IdxVec<ir::DefIdx, HashSet<ir::DefIdx>>,
}

#[derive(Debug)]
pub(super) struct ControlGraph {
    def_idx: ir::DefIdx,
    entry: ir::BlockIdx,
    preds: IdxVec<ir::BlockIdx, Option<ir::BlockIdx>>,
    succs: IdxVec<ir::BlockIdx, HashSet<ir::BlockIdx>>,
    returns: HashSet<ir::BlockIdx>,
}

impl CallGraph {
    pub(super) fn new(ir: &ir::Ir) -> CallGraph {
        let mut call_graph = CallGraph {
            control_graphs: IdxVec::with_capacity(ir.defs.len()),
            callers: IdxVec::with_capacity(ir.defs.len()),
        };
        for def in ir.defs.values() {
            call_graph.control_graphs.push(ControlGraph::new(def));
            call_graph.callers.push(HashSet::new());
        }
        for def in ir.defs.values() {
            call_graph.register_block(&def.entry.body);
        }
        call_graph
    }

    fn register_block(&mut self, block: &ir::Block) {
        for instruction in block.instructions.iter() {
            if let ir::InstructionKind::Let {
                expr:
                    ir::Expr {
                        kind: ir::ExprKind::Call { target, .. },
                        ..
                    },
                ..
            } = &instruction.kind
            {
                self.callers[*target].insert(block.owner);
            }
        }
        if let ir::Terminator::Match { arms, .. } = &block.terminator {
            for arm in arms.iter() {
                self.register_block(&arm.target);
            }
        }
    }

    #[inline]
    pub(super) fn callers(&self, idx: ir::DefIdx) -> impl Iterator<Item = &ir::DefIdx> {
        self.callers[idx].iter()
    }

    #[inline]
    pub(super) fn control_graph(&self, idx: ir::DefIdx) -> &ControlGraph {
        &self.control_graphs[idx]
    }
}

impl ControlGraph {
    fn new(def: &ir::Def) -> ControlGraph {
        let mut control_graph = ControlGraph {
            def_idx: def.def_idx,
            entry: def.entry.body.block_idx,
            preds: IdxVec::new(),
            succs: IdxVec::new(),
            returns: HashSet::new(),
        };
        control_graph.register_block(&def.entry.body, None);
        control_graph
    }

    fn register_block(&mut self, block: &ir::Block, pred: Option<ir::BlockIdx>) -> ir::BlockIdx {
        let idx = self.preds.push(pred);
        self.succs.push(HashSet::new());
        match &block.terminator {
            ir::Terminator::Return(_) => {
                self.returns.insert(idx);
            }
            ir::Terminator::Match { arms, .. } => {
                for arm in arms.iter() {
                    let target = self.register_block(&arm.target, Some(idx));
                    self.succs[idx].insert(target);
                }
            }
        }
        debug_assert_eq!(idx, block.block_idx);
        idx
    }

    #[inline]
    pub(super) fn entry(&self) -> ir::BlockIdx {
        self.entry
    }

    #[inline]
    pub(super) fn pred(&self, idx: ir::BlockIdx) -> Option<ir::BlockIdx> {
        self.preds[idx]
    }

    #[inline]
    pub(super) fn succs(&self, idx: ir::BlockIdx) -> impl Iterator<Item = &ir::BlockIdx> {
        self.succs[idx].iter()
    }

    #[inline]
    pub(super) fn returns(&self) -> impl Iterator<Item = &ir::BlockIdx> {
        self.returns.iter()
    }
}
