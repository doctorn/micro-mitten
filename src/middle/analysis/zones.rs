use crate::middle::analysis::paths::Path;
use crate::middle::analysis::{DataFlow, LocalAnalysisEngine};
use crate::middle::ir;
use crate::ty;

use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub struct Zone<T> {
    idx: T,
    path: Path,
}

#[derive(Clone)]
pub struct ZoneSet<T> {
    idx: T,
    ty: ty::Ty,
    paths: Vec<Path>,
}

impl<T> Zone<T>
where
    T: Copy + Eq + 'static,
{
    /// Marked as unsafe as the index isn't necessarily type compatible with the path and
    /// there is no way of asserting this
    pub(super) unsafe fn with_idx_and_path(t: T, path: Path) -> Zone<T> {
        Zone { idx: t, path }
    }

    #[inline]
    pub fn with_idx<U>(self, u: U) -> Zone<U> {
        Zone {
            idx: u,
            path: self.path,
        }
    }

    #[inline]
    pub fn compatible_with(&self, other: &Zone<T>) -> bool {
        self.idx == other.idx && self.path.compatible_with(&other.path)
    }

    #[inline]
    pub fn is_prefix(&self, other: &Zone<T>) -> bool {
        self.idx == other.idx && self.path.is_prefix(&other.path)
    }

    #[inline]
    pub fn subsumes(&self, other: &Zone<T>) -> bool {
        self.idx == other.idx && self.path.subsumes(&other.path)
    }

    #[inline]
    pub fn idx(&self) -> T {
        self.idx
    }

    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn into_path(self) -> Path {
        self.path
    }
}

impl<T> Zone<T>
where
    T: Copy + fmt::Debug + Eq + 'static,
{
    pub fn or(&self, other: &Zone<T>) -> Zone<T> {
        debug_assert_eq!(self.idx, other.idx);
        Zone {
            idx: self.idx,
            path: self.path.or(&other.path),
        }
    }
}

impl<T> fmt::Debug for Zone<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}, {:?})", self.idx, self.path)
    }
}

impl<T> ZoneSet<T>
where
    T: Copy + Eq + 'static,
{
    pub(super) fn with_idx<U>(self, u: U) -> ZoneSet<U> {
        ZoneSet {
            idx: u,
            ty: self.ty,
            paths: self.paths,
        }
    }

    pub(super) fn add_path(&mut self, other: Path) {
        debug_assert_eq!(self.ty, other.src);
        for i in 0..self.paths.len() {
            if self.paths[i].compatible_with(&other) {
                self.paths[i] = self.paths[i].or(&other);
                return;
            }
        }
        self.paths.push(other);
    }

    pub(super) fn add_path_ignoring_prefixes(&mut self, other: Path) {
        debug_assert_eq!(self.ty, other.src);
        for i in 0..self.paths.len() {
            if other.is_prefix(&self.paths[i]) {
                return;
            } else if self.paths[i].is_prefix(&other) {
                self.paths[i] = other;
                return;
            }
        }
        self.add_path(other)
    }

    #[inline]
    pub fn idx(&self) -> T {
        self.idx
    }

    pub fn subsumes(&self, path: &Path) -> bool {
        for i in 0..self.paths.len() {
            if self.paths[i].subsumes(path) {
                return true;
            }
        }
        false
    }

    #[inline]
    pub fn paths(&self) -> impl Iterator<Item = &Path> {
        self.paths.iter()
    }

    #[inline]
    pub fn zones<'a>(&'a self) -> impl Iterator<Item = Zone<T>> + 'a {
        let idx = self.idx;
        self.paths
            .iter()
            .cloned()
            .map(move |path| Zone { idx, path })
    }

    #[inline]
    pub fn into_paths(self) -> impl Iterator<Item = Path> {
        self.paths.into_iter()
    }

    #[inline]
    pub fn into_zones(self) -> impl Iterator<Item = Zone<T>> {
        let idx = self.idx;
        self.paths.into_iter().map(move |path| Zone { idx, path })
    }
}

impl<T> ZoneSet<T>
where
    T: Copy + fmt::Debug + Eq + 'static,
{
    pub(super) fn add_zone(&mut self, other: Zone<T>) {
        debug_assert_eq!(self.idx, other.idx);
        self.add_path(other.into_path());
    }

    pub(super) fn add_zone_ignoring_prefixes(&mut self, other: Zone<T>) {
        debug_assert_eq!(self.idx, other.idx);
        self.add_path_ignoring_prefixes(other.into_path());
    }

    pub(super) fn merge(&mut self, other: ZoneSet<T>) {
        debug_assert_eq!(self.idx, other.idx);
        for path in other.into_paths() {
            self.add_path(path);
        }
    }
}

impl<T> PartialEq for ZoneSet<T>
where
    T: Copy + Eq + 'static,
{
    fn eq(&self, other: &ZoneSet<T>) -> bool {
        if self.idx != other.idx {
            return false;
        }
        for path in other.paths() {
            if !self.paths.contains(path) {
                return false;
            }
        }
        for path in self.paths() {
            if !other.paths.contains(path) {
                return false;
            }
        }
        true
    }
}

impl<T> Eq for ZoneSet<T> where T: Copy + Eq + 'static {}

impl<T> From<Zone<T>> for ZoneSet<T> {
    fn from(zone: Zone<T>) -> ZoneSet<T> {
        ZoneSet {
            idx: zone.idx,
            ty: zone.path.src,
            paths: vec![zone.path],
        }
    }
}

impl<T> fmt::Debug for ZoneSet<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?}, ", self.idx)?;
        f.debug_set().entries(self.paths.iter()).finish()?;
        write!(f, ")")
    }
}

impl<'ir, 'eng, T> LocalAnalysisEngine<'ir, 'eng, T>
where
    T: DataFlow,
{
    #[inline]
    pub(super) fn epsilon_path(&self, idx: ir::LocalIdx) -> Path {
        Path::epsilon(self.ty_env()[self.def()][idx])
    }

    #[inline]
    pub(super) fn epsilon_zone(&self, idx: ir::LocalIdx) -> Zone<ir::LocalIdx> {
        Zone {
            idx,
            path: self.epsilon_path(idx),
        }
    }

    #[inline]
    pub(super) fn zone_from_path(&self, idx: ir::LocalIdx, path: Path) -> Zone<ir::LocalIdx> {
        debug_assert_eq!(self.ty_env()[self.def()][idx], path.src);
        Zone { idx, path }
    }
}
