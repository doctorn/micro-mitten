use crate::common::{Idx, IdxVec, Map, Set};
use crate::middle::analysis::paths::Path;
use crate::middle::analysis::zones::Zone;
use crate::middle::analysis::{AnalysisLoc, DataFlow, DataFlowDir, LocalAnalysisEngine};
use crate::middle::ir;
use crate::ty;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

#[derive(Clone, PartialEq, Eq)]
pub struct ShapeSet<T, U>
where
    T: Copy + Eq,
    U: Copy + Eq + Hash + 'static,
{
    idx: T,
    shape: Map<Path, HashMap<U, Set<Path>>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ShapeDecoration<T>
where
    T: Copy + Eq + Hash + 'static,
{
    shape: HashMap<T, ShapeSet<T, T>>,
}

impl<T, U> ShapeSet<T, U>
where
    T: Copy + Eq + 'static,
    U: Copy + Eq + Hash + 'static,
{
    fn new(idx: T) -> ShapeSet<T, U> {
        ShapeSet {
            idx,
            shape: Map::new(),
        }
    }

    fn with_idx<S>(self, idx: S) -> ShapeSet<S, U>
    where
        S: Copy + Eq,
    {
        ShapeSet {
            idx,
            shape: self.shape,
        }
    }

    fn shapes(&self, shape_path: &Path, other: &Zone<U>) -> bool {
        self.shape
            .iter()
            .filter(move |(path, _)| path.is_prefix(shape_path))
            .flat_map(|(_, shape_record)| {
                shape_record
                    .get(&other.idx())
                    .into_iter()
                    .flat_map(|record| record.iter())
            })
            .any(|path| path.is_prefix(other.path()))
    }

    fn shaping_zones<'a>(&'a self, shape_path: &'a Path) -> impl Iterator<Item = Zone<U>> + 'a {
        self.shape
            .iter()
            .filter(move |(path, _)| path.is_prefix(shape_path))
            .flat_map(move |(path, shape_record)| {
                shape_record.iter().flat_map(move |(u, path_set)| {
                    path_set.iter().map(move |other_path| {
                        let composite_path = other_path.then(&shape_path.subtract_prefix(path));
                        unsafe { Zone::with_idx_and_path(*u, composite_path) }
                    })
                })
            })
    }

    fn add(&mut self, path: Path, other: Zone<U>) {
        if let Some(shape_record) = self.shape.get_mut(&path) {
            if let Some(path_set) = shape_record.get_mut(&other.idx()) {
                path_set.insert(other.into_path());
            } else {
                shape_record.insert(other.idx(), Set::singleton(other.into_path()));
            }
        } else {
            self.shape.insert(path, {
                let mut shape_record = HashMap::new();
                shape_record.insert(other.idx(), Set::singleton(other.into_path()));
                shape_record
            });
        }
    }

    fn merge(&mut self, other: ShapeSet<T, U>) {
        for (path, other_record) in other.shape.into_iter() {
            if let Some(shape_record) = self.shape.get_mut(&path) {
                for (idx, other_set) in other_record.into_iter() {
                    if let Some(path_set) = shape_record.get_mut(&idx) {
                        path_set.insert_all(other_set.into_iter());
                    } else {
                        shape_record.insert(idx, other_set);
                    }
                }
            } else {
                self.shape.insert(path, other_record);
            }
        }
    }

    fn pairings(&self) -> impl Iterator<Item = ((T, &Path), (U, &Path))> {
        let idx = self.idx;
        self.shape.iter().flat_map(move |(path, shape_record)| {
            shape_record.iter().flat_map(move |(u, path_set)| {
                path_set
                    .iter()
                    .map(move |other_path| ((idx, path), (*u, other_path)))
            })
        })
    }
}

impl<T, U> ShapeSet<T, U>
where
    T: Copy + Eq + 'static,
    U: Idx,
{
    fn forward_subst<V>(self, subst: &IdxVec<U, V>) -> ShapeSet<T, V>
    where
        V: Copy + Eq + Hash + 'static,
    {
        let mut shape_set = ShapeSet::new(self.idx);
        for (path, mut shape_record) in self.shape.into_iter() {
            let mut subst_record = HashMap::new();
            for (u, v) in subst.iter() {
                if let Some(path_set) = shape_record.remove(&u) {
                    subst_record.insert(*v, path_set);
                }
            }
            shape_set.shape.insert(path, subst_record);
        }
        shape_set
    }

    fn backward_subst<V>(self, subst: &HashMap<U, HashSet<V>>) -> ShapeSet<T, V>
    where
        V: Copy + Eq + Hash + 'static,
    {
        let mut shape_set = ShapeSet::new(self.idx);
        for (path, mut shape_record) in self.shape.into_iter() {
            let mut subst_record: HashMap<_, Set<Path>> = HashMap::new();
            for (u, set) in subst.iter() {
                if let Some(path_set) = shape_record.remove(&u) {
                    for v in set.iter() {
                        if let Some(subst_set) = subst_record.get_mut(v) {
                            subst_set.insert_all(path_set.iter().cloned());
                        } else {
                            subst_record.insert(*v, path_set.clone());
                        }
                    }
                }
            }
            shape_set.shape.insert(path, subst_record);
        }
        shape_set
    }
}

impl<T, U> fmt::Debug for ShapeSet<T, U>
where
    T: Copy + fmt::Debug + Eq + 'static,
    U: Copy + fmt::Debug + Eq + Hash + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set().entries(self.pairings()).finish()
    }
}

impl<T> ShapeDecoration<T>
where
    T: Copy + Eq + Hash + 'static,
{
    fn new() -> ShapeDecoration<T> {
        ShapeDecoration {
            shape: HashMap::new(),
        }
    }

    pub fn shapes(&self, left: &Zone<T>, right: &Zone<T>) -> bool {
        left.subsumes(right)
            || right.subsumes(left)
            || self
                .shape
                .get(&left.idx())
                .map(|shape_set| shape_set.shapes(left.path(), right))
                .unwrap_or(false)
    }

    pub fn shaping_zones<'a>(&'a self, zone: &'a Zone<T>) -> impl Iterator<Item = Zone<T>> + 'a {
        self.shape
            .get(&zone.idx())
            .into_iter()
            .flat_map(move |shape_set| shape_set.shaping_zones(zone.path()))
    }

    fn add_asymmetric(&mut self, left: Zone<T>, right: Zone<T>) {
        if let Some(shape_set) = self.shape.get_mut(&left.idx()) {
            let suffix_zones = shape_set.shaping_zones(left.path()).collect::<Vec<_>>();
            shape_set.add(left.path().clone(), right.clone());
            for zone in suffix_zones {
                self.add(zone, right.clone());
            }
        } else {
            let idx = left.idx();
            let mut shape_set = ShapeSet::new(idx);
            shape_set.add(left.into_path(), right);
            self.shape.insert(idx, shape_set);
        }
    }

    fn add(&mut self, left: Zone<T>, right: Zone<T>) {
        debug_assert_eq!(left.path().dest, right.path().dest);
        if !self.shapes(&left, &right) {
            self.add_asymmetric(left.clone(), right.clone());
            self.add_asymmetric(right, left);
        }
    }

    fn merge_on<U>(&mut self, idx: T, other_set: ShapeSet<U, T>)
    where
        U: Copy + Eq + 'static,
    {
        if let Some(shape_set) = self.shape.get_mut(&idx) {
            shape_set.merge(other_set.with_idx(idx));
        } else {
            self.shape.insert(idx, other_set.with_idx(idx));
        }
    }

    fn merge(&mut self, other: ShapeDecoration<T>) {
        for (idx, shape_set) in other.shape.into_iter() {
            self.merge_on(idx, shape_set);
        }
    }

    fn summary_from(&self, t: T) -> ShapeSet<(), T> {
        if let Some(shape_set) = self.shape.get(&t) {
            shape_set.clone().with_idx(())
        } else {
            ShapeSet::new(())
        }
    }
}

impl<T> ShapeDecoration<T>
where
    T: Idx,
{
    fn forward_subst<U>(mut self, subst: &IdxVec<T, U>) -> ShapeDecoration<U>
    where
        U: Copy + Eq + Hash + 'static,
    {
        let mut decoration = ShapeDecoration::new();
        for (t, u) in subst.iter() {
            if let Some(shape_set) = self.shape.remove(&t) {
                decoration
                    .shape
                    .insert(*u, shape_set.forward_subst(subst).with_idx(*u));
            }
        }
        decoration
    }

    fn backward_subst<U>(mut self, subst: &HashMap<T, HashSet<U>>) -> ShapeDecoration<U>
    where
        U: Copy + Eq + Hash + 'static,
    {
        let mut decoration = ShapeDecoration::new();
        for (t, set) in subst.iter() {
            if let Some(shape_set) = self.shape.remove(&t) {
                for u in set.iter() {
                    if let Some(subst_set) = decoration.shape.get_mut(u) {
                        subst_set.merge(shape_set.clone().backward_subst(subst).with_idx(*u));
                    } else {
                        decoration
                            .shape
                            .insert(*u, shape_set.clone().backward_subst(subst).with_idx(*u));
                    }
                }
            }
        }
        decoration
    }
}

impl<T> fmt::Debug for ShapeDecoration<T>
where
    T: Copy + fmt::Debug + Eq + Hash + 'static,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set()
            .entries(self.shape.values().flat_map(ShapeSet::pairings))
            .finish()
    }
}

pub struct ImpliedAccess;

impl DataFlow for ImpliedAccess {
    const DIR: DataFlowDir = DataFlowDir::Forward;

    type Context = ShapeDecoration<ty::ParamIdx>;
    type Summary = ShapeSet<(), ty::ParamIdx>;
    type Decoration = ShapeDecoration<ir::LocalIdx>;

    #[inline]
    fn empty_context() -> Self::Context {
        ShapeDecoration::new()
    }

    #[inline]
    fn empty_summary() -> Self::Summary {
        ShapeSet::new(())
    }

    #[inline]
    fn empty_decoration() -> Self::Decoration {
        ShapeDecoration::new()
    }

    fn entry(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
    ) {
        *dec = engine
            .context(engine.def())
            .clone()
            .forward_subst(engine.param_bindings());
    }

    fn expr(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
        binding: ir::LocalIdx,
        expr: &ir::ExprKind,
    ) {
        match expr {
            ir::ExprKind::Var(idx)
                if engine.ty_env()[engine.def()][*idx] != engine.ty_sess().mk_u64() =>
            {
                dec.add(engine.epsilon_zone(binding), engine.epsilon_zone(*idx));
            }
            ir::ExprKind::Call { target, args, .. } => {
                let summary = engine.summary(*target).clone().forward_subst(args);
                dec.merge_on(binding, summary);
                let mut context = { engine.context(*target).clone() };
                let subst = args.invert();
                context.merge(dec.clone().backward_subst(&subst));
                // NOTE this is a very messy solution to the fact that we need to alias between formal
                // parameters that are instantiated with the same local indeces, I'm not really sure how
                // else to do this at the moment, but this definitely wants tidying up
                for (local_idx, param_idx_set) in subst.into_iter() {
                    for idx in param_idx_set.iter() {
                        for other_idx in param_idx_set.iter().filter(|other_idx| *other_idx != idx)
                        {
                            if engine.ty_env()[engine.def()][local_idx] != engine.ty_sess().mk_u64()
                            {
                                context.add(
                                    engine.epsilon_zone(local_idx).with_idx(*idx),
                                    engine.epsilon_zone(local_idx).with_idx(*other_idx),
                                )
                            }
                        }
                    }
                }
                engine.update_context(*target, context);
            }
            ir::ExprKind::Variant {
                body: idx,
                discriminant,
                ..
            } if engine.ty_env()[engine.def()][*idx] != engine.ty_sess().mk_u64() => dec.add(
                engine.zone_from_path(
                    binding,
                    engine
                        .epsilon_path(binding)
                        .variant(engine.ty_sess(), *discriminant),
                ),
                engine.epsilon_zone(*idx),
            ),
            ir::ExprKind::Record { fields, .. } => {
                for (field, idx) in fields.iter() {
                    if engine.ty_env()[engine.def()][*idx] != engine.ty_sess().mk_u64() {
                        dec.add(
                            engine.zone_from_path(
                                binding,
                                engine.epsilon_path(binding).field(engine.ty_sess(), field),
                            ),
                            engine.epsilon_zone(*idx),
                        )
                    }
                }
            }
            _ => {}
        }
    }

    fn pattern(
        engine: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        dec: &mut Self::Decoration,
        source: ir::LocalIdx,
        pattern: &ir::PatternKind,
    ) {
        match pattern {
            ir::PatternKind::Ident(idx)
                if engine.ty_env()[engine.def()][*idx] != engine.ty_sess().mk_u64() =>
            {
                dec.add(engine.epsilon_zone(source), engine.epsilon_zone(*idx));
            }
            ir::PatternKind::Variant {
                binding,
                discriminant,
                ..
            } if engine.ty_env()[engine.def()][*binding] != engine.ty_sess().mk_u64() => dec.add(
                engine.zone_from_path(
                    source,
                    engine
                        .epsilon_path(source)
                        .variant(engine.ty_sess(), *discriminant),
                ),
                engine.epsilon_zone(*binding),
            ),
            ir::PatternKind::Record { fields, .. } => {
                for (field, idx) in fields.iter() {
                    if engine.ty_env()[engine.def()][*idx] != engine.ty_sess().mk_u64() {
                        dec.add(
                            engine.zone_from_path(
                                source,
                                engine.epsilon_path(source).field(engine.ty_sess(), field),
                            ),
                            engine.epsilon_zone(*idx),
                        )
                    }
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
        let mut summary = { engine.summary(engine.def()).clone() };
        summary.merge(
            dec.summary_from(idx)
                .backward_subst(&engine.param_bindings().invert()),
        );
        for (param, arg) in engine.param_bindings().iter() {
            if idx == *arg {
                summary.add(
                    engine.epsilon_path(idx),
                    engine.epsilon_zone(idx).with_idx(param),
                )
            }
        }
        engine.update_summary(engine.def(), summary);
    }

    fn conflate(
        _: &LocalAnalysisEngine<'_, '_, Self>,
        _: AnalysisLoc,
        _: Vec<Self::Decoration>,
    ) -> Self::Decoration {
        unreachable!("forwards analyses shouldn't reach merge points in ANF")
    }
}

impl<I, T> IdxVec<I, T>
where
    I: Idx,
    T: Idx,
{
    fn invert(&self) -> HashMap<T, HashSet<I>> {
        let mut inversion: HashMap<_, HashSet<I>> = HashMap::new();
        for (i, t) in self.iter() {
            if let Some(set) = inversion.get_mut(t) {
                set.insert(i);
            } else {
                inversion.insert(*t, {
                    let mut set = HashSet::new();
                    set.insert(i);
                    set
                });
            }
        }
        inversion
    }
}
