use crate::middle::analysis::{AnalysisLoc, DataFlow, DataFlowDir, LocalAnalysisEngine};
use crate::middle::ir;
use crate::ty;

use std::collections::HashSet;

pub struct Lva;

impl DataFlow for Lva {
    const DIR: DataFlowDir = DataFlowDir::Backward;

    type Context = bool;
    type Summary = HashSet<ty::ParamIdx>;
    type Decoration = HashSet<ir::LocalIdx>;

    #[inline]
    fn empty_context() -> Self::Context {
        false
    }

    #[inline]
    fn empty_summary() -> Self::Summary {
        HashSet::new()
    }

    #[inline]
    fn empty_decoration() -> Self::Decoration {
        HashSet::new()
    }

    fn entry(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
    ) {
        let mut summary = engine.summary(engine.def()).clone();
        for (idx, param) in engine.param_bindings().iter() {
            if dec.remove(param) {
                summary.insert(idx);
            }
        }
        engine.update_summary(engine.def(), summary);
    }

    fn expr(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
        binding: ir::LocalIdx,
        expr: &ir::ExprKind,
    ) {
        if dec.remove(&binding) {
            match expr {
                ir::ExprKind::Var(idx)
                | ir::ExprKind::Unop { operand: idx, .. }
                | ir::ExprKind::Variant { body: idx, .. } => {
                    dec.insert(*idx);
                }
                ir::ExprKind::Binop { left, right, .. } => {
                    dec.insert(*left);
                    dec.insert(*right);
                }
                ir::ExprKind::Call { target, args } => {
                    let summary = engine.summary(*target);
                    for (idx, arg) in args.iter() {
                        if summary.contains(&idx) {
                            dec.insert(*arg);
                        }
                    }
                    engine.update_context(*target, true);
                }
                ir::ExprKind::Record { fields, .. } => {
                    for field in fields.values() {
                        dec.insert(*field);
                    }
                }
                _ => {}
            }
        }
    }

    fn pattern(
        _: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
        source: ir::LocalIdx,
        pattern: &ir::PatternKind,
    ) {
        dec.insert(source);
        match pattern {
            ir::PatternKind::Ident(idx) | ir::PatternKind::Variant { binding: idx, .. } => {
                dec.remove(idx);
            }
            ir::PatternKind::Record { fields, .. } => {
                for field in fields.values() {
                    dec.remove(field);
                }
            }
            _ => {}
        }
    }

    fn ret(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
        idx: ir::LocalIdx,
    ) {
        if *engine.context(engine.def()) {
            dec.insert(idx);
        } else if engine.def() == engine.entry() {
            dec.insert(idx);
            engine.update_context(engine.def(), true);
        }
    }

    fn conflate(
        _: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        decorations: Vec<Self::Decoration>,
    ) -> Self::Decoration {
        let mut union = HashSet::new();
        for set in decorations.into_iter() {
            for live in set {
                union.insert(live);
            }
        }
        union
    }
}
