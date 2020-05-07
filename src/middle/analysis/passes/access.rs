use crate::middle::analysis::passes::{ImpliedAccess, ShapeDecoration};
use crate::middle::analysis::zones::{Zone, ZoneSet};
use crate::middle::analysis::{AnalysisLoc, DataFlow, DataFlowDir, LocalAnalysisEngine};
use crate::middle::ir;
use crate::ty;

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

pub struct Access;

#[derive(Clone, PartialEq, Eq)]
pub struct AccessDecoration<T>
where
    T: Copy + Eq + Hash + 'static,
{
    access: HashMap<T, ZoneSet<T>>,
}

impl<T> AccessDecoration<T>
where
    T: Copy + fmt::Debug + Eq + Hash + 'static,
{
    pub fn new() -> AccessDecoration<T> {
        AccessDecoration {
            access: HashMap::new(),
        }
    }

    pub fn add(&mut self, zone: Zone<T>) {
        if let Some(zone_set) = self.access.get_mut(&zone.idx()) {
            zone_set.add_zone(zone);
        } else {
            self.access.insert(zone.idx(), zone.into());
        }
    }

    pub fn add_ignoring_prefixes(&mut self, zone: Zone<T>) {
        if let Some(zone_set) = self.access.get_mut(&zone.idx()) {
            zone_set.add_zone_ignoring_prefixes(zone);
        } else {
            self.access.insert(zone.idx(), zone.into());
        }
    }

    fn close(&mut self, implied: &ShapeDecoration<T>) {
        loop {
            let prev = self.clone();
            for zone in prev.zones() {
                for other in implied.shaping_zones(&zone) {
                    self.add(other);
                }
            }
            if &prev == self {
                break;
            }
        }
    }

    fn merge(&mut self, other: AccessDecoration<T>) {
        for (idx, other) in other.access.into_iter() {
            if let Some(zone_set) = self.access.get_mut(&idx) {
                zone_set.merge(other);
            } else {
                self.access.insert(other.idx(), other);
            }
        }
    }

    pub fn anti_matter_with(
        &self,
        ty_sess: &ty::TySess,
        other: &AccessDecoration<T>,
    ) -> AccessDecoration<T> {
        let mut diff = AccessDecoration::new();
        for zone in self.zones() {
            if zone.path().src != ty_sess.mk_u64()
                && zone.path().dest != ty_sess.mk_u64()
                && !other.accessed_on(&zone)
            {
                diff.add(zone)
            }
        }
        diff
    }

    pub fn matter_with(
        &self,
        ty_sess: &ty::TySess,
        other: &AccessDecoration<T>,
        implied: &ShapeDecoration<T>,
        filter: impl Fn(&Zone<T>) -> bool,
    ) -> AccessDecoration<T> {
        let mut diff = AccessDecoration::new();
        for zone in self.zones() {
            if zone.path().src != ty_sess.mk_u64()
                && zone.path().dest != ty_sess.mk_u64()
                && other.zones().any(|other| implied.shapes(&zone, &other))
                && filter(&zone)
            {
                diff.add_ignoring_prefixes(zone)
            }
        }
        diff
    }

    #[inline]
    pub fn zones(&self) -> impl Iterator<Item = Zone<T>> + '_ {
        self.access
            .iter()
            .flat_map(|(_, zone_set)| zone_set.zones())
    }

    #[inline]
    pub fn accessed(&self, t: T) -> bool {
        self.access.contains_key(&t)
    }

    #[inline]
    pub fn accessed_on(&self, zone: &Zone<T>) -> bool {
        self.access
            .get(&zone.idx())
            .map(|zone_set| zone_set.subsumes(zone.path()))
            .unwrap_or_default()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.access.is_empty()
    }

    #[inline]
    pub fn get(&self, idx: T) -> Option<&ZoneSet<T>> {
        self.access.get(&idx)
    }

    #[inline]
    fn remove(&mut self, idx: T) -> Option<ZoneSet<T>> {
        self.access.remove(&idx)
    }
}

impl<T> fmt::Debug for AccessDecoration<T>
where
    T: Copy + fmt::Debug + Eq + Hash + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set().entries(self.access.values()).finish()
    }
}

impl DataFlow for Access {
    const DIR: DataFlowDir = DataFlowDir::Backward;

    type Context = (Option<ZoneSet<()>>, AccessDecoration<ty::ParamIdx>);
    type Summary = AccessDecoration<ty::ParamIdx>;
    type Decoration = AccessDecoration<ir::LocalIdx>;

    #[inline]
    fn empty_context() -> Self::Context {
        (None, AccessDecoration::new())
    }

    #[inline]
    fn empty_summary() -> Self::Summary {
        AccessDecoration::new()
    }

    #[inline]
    fn empty_decoration() -> Self::Decoration {
        AccessDecoration::new()
    }

    fn entry(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
    ) {
        let mut summary = { engine.summary(engine.def()).clone() };
        for (idx, param) in engine.param_bindings().iter() {
            if let Some(zone_set) = dec.remove(*param) {
                for zone in zone_set.with_idx(idx).into_zones() {
                    summary.add(zone);
                }
            }
        }
        engine.update_summary(engine.def(), summary);
    }

    fn expr(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        binding: ir::LocalIdx,
        expr: &ir::ExprKind,
    ) {
        match expr {
            ir::ExprKind::Var(idx)
            | ir::ExprKind::Unop { operand: idx, .. }
            | ir::ExprKind::Variant { body: idx, .. } => {
                dec.add(engine.epsilon_zone(*idx));
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(binding);
            }
            ir::ExprKind::Binop { left, right, .. } => {
                dec.add(engine.epsilon_zone(*left));
                dec.add(engine.epsilon_zone(*right));
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(binding);
            }
            ir::ExprKind::Call { target, args } => {
                let summary = engine.summary(*target);
                for (idx, arg) in args.iter() {
                    if let Some(param) = summary.get(idx) {
                        for path in param.paths() {
                            dec.add(engine.zone_from_path(*arg, path.clone()));
                        }
                    }
                    dec.add(engine.epsilon_zone(*arg));
                }
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                let mut context = { engine.context(*target).clone() };
                if let Some(binding_zone_set) = dec.remove(binding) {
                    if let Some(ref mut context) = context.0 {
                        context.merge(binding_zone_set.with_idx(()));
                    } else {
                        context.0 = Some(binding_zone_set.with_idx(()));
                    }
                }
                for (idx, arg) in args.iter() {
                    if let Some(zone_set) = dec.get(*arg) {
                        for zone in zone_set.clone().with_idx(idx).into_zones() {
                            context.1.add(zone);
                        }
                    }
                }
                engine.update_context(*target, context);
            }
            ir::ExprKind::Record { fields, .. } => {
                for field in fields.values() {
                    dec.add(engine.epsilon_zone(*field));
                }
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(binding);
            }
            _ => {
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(binding);
            }
        }
    }

    fn pattern(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        source: ir::LocalIdx,
        pattern: &ir::PatternKind,
    ) {
        match pattern {
            ir::PatternKind::Ident(idx) => {
                dec.add(engine.epsilon_zone(source));
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(*idx);
            }
            ir::PatternKind::Variant {
                binding,
                discriminant,
                ..
            } => {
                dec.add(
                    engine.zone_from_path(
                        source,
                        engine
                            .epsilon_path(source)
                            .variant(engine.ty_sess(), *discriminant),
                    ),
                );
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                dec.remove(*binding);
            }
            ir::PatternKind::Record { fields, .. } => {
                for field in fields.keys() {
                    dec.add(engine.zone_from_path(
                        source,
                        engine.epsilon_path(source).field(engine.ty_sess(), field),
                    ));
                }
                dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
                for idx in fields.values() {
                    dec.remove(*idx);
                }
            }
            _ => {}
        }
    }

    fn ret(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        dec: &mut Self::Decoration,
        idx: ir::LocalIdx,
    ) {
        let context = { engine.context(engine.def()).clone() };
        for (idx, param) in engine.param_bindings().iter() {
            if let Some(zone_set) = context.1.get(idx) {
                for zone in zone_set.clone().with_idx(*param).into_zones() {
                    dec.add(zone);
                }
            }
        }
        if let Some(zone_set) = context.0 {
            for zone in zone_set.with_idx(idx).into_zones() {
                dec.add(zone);
            }
        } else if engine.def() == engine.entry() {
            engine.update_context(
                engine.def(),
                (
                    Some(engine.epsilon_zone(idx).with_idx(()).into()),
                    AccessDecoration::new(),
                ),
            );
        }
        dec.add(engine.epsilon_zone(idx));
        dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
    }

    fn conflate(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        loc: AnalysisLoc,
        mut decorations: Vec<Self::Decoration>,
    ) -> Self::Decoration {
        debug_assert_ne!(decorations.len(), 0);
        let mut dec = decorations.remove(0);
        for other in decorations.into_iter() {
            dec.merge(other);
        }
        dec.close(engine.pass::<ImpliedAccess>().lookup(loc));
        dec
    }
}
