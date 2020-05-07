use crate::ty;

use std::collections::HashSet;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Atom {
    Field(ty::FieldIdx),
    Variant(ty::VariantIdx),
}

impl fmt::Debug for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Field(idx) => write!(f, "{:?}", idx),
            Atom::Variant(idx) => write!(f, "{:?}", idx),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct Edge {
    src: ty::Ty,
    dest: ty::Ty,
    atom: Atom,
}

impl fmt::Debug for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.atom)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Path {
    pub src: ty::Ty,
    pub dest: ty::Ty,
    edges: HashSet<Edge>,
}

impl Path {
    #[inline]
    pub fn epsilon(ty: ty::Ty) -> Path {
        Path {
            src: ty,
            dest: ty,
            edges: HashSet::new(),
        }
    }

    pub fn field(&self, sess: &ty::TySess, idx: ty::FieldIdx) -> Path {
        let dest = sess
            .ty_kind(self.dest)
            .field_ty(idx)
            .expect("path does not support field access");
        Path {
            src: self.src,
            dest,
            edges: {
                let mut edges = self.edges.clone();
                edges.insert(Edge {
                    src: self.dest,
                    dest,
                    atom: Atom::Field(idx),
                });
                edges
            },
        }
    }

    pub fn variant(&self, sess: &ty::TySess, idx: ty::VariantIdx) -> Path {
        let dest = sess
            .ty_kind(self.dest)
            .variant_ty(idx)
            .expect("path does not support variant access");
        Path {
            src: self.src,
            dest,
            edges: {
                let mut edges = self.edges.clone();
                edges.insert(Edge {
                    src: self.dest,
                    dest,
                    atom: Atom::Variant(idx),
                });
                edges
            },
        }
    }

    #[inline]
    fn union_edges(&self, other: &Path) -> HashSet<Edge> {
        self.edges.union(&other.edges).copied().collect()
    }

    #[inline]
    fn subtract_edges(&self, other: &Path) -> HashSet<Edge> {
        self.edges.difference(&other.edges).copied().collect()
    }

    pub fn then(&self, other: &Path) -> Path {
        debug_assert_eq!(self.dest, other.src);
        Path {
            src: self.src,
            dest: other.dest,
            edges: self.union_edges(other),
        }
    }

    pub fn subtract_prefix(&self, other: &Path) -> Path {
        debug_assert!(other.is_prefix(self));
        Path {
            src: other.dest,
            dest: self.dest,
            edges: self.subtract_edges(other),
        }
    }

    pub fn subtract_suffix(&self, other: &Path) -> Path {
        debug_assert!(other.is_suffix(self));
        Path {
            src: self.src,
            dest: other.src,
            edges: self.subtract_edges(other),
        }
    }

    pub fn or(&self, other: &Path) -> Path {
        debug_assert!(self.compatible_with(other));
        Path {
            src: self.src,
            dest: self.dest,
            edges: self.union_edges(other),
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.edges.is_empty()
    }

    pub fn contains_field_edge(&self, src: ty::Ty, idx: ty::FieldIdx) -> bool {
        self.edges
            .iter()
            .any(move |edge| edge.src == src && edge.atom == Atom::Field(idx))
    }

    pub fn contains_variant_edge(&self, src: ty::Ty, idx: ty::VariantIdx) -> bool {
        self.edges
            .iter()
            .any(move |edge| edge.src == src && edge.atom == Atom::Variant(idx))
    }

    #[inline]
    pub fn compatible_with(&self, other: &Path) -> bool {
        self.src == other.src && self.dest == other.dest
    }

    #[inline]
    pub fn is_prefix(&self, other: &Path) -> bool {
        self.src == other.src && self.edges.is_subset(&other.edges)
    }

    #[inline]
    pub fn is_suffix(&self, other: &Path) -> bool {
        self.dest == other.dest && self.edges.is_subset(&other.edges)
    }

    #[inline]
    pub fn subsumes(&self, other: &Path) -> bool {
        self.compatible_with(other) && self.edges.is_superset(&other.edges)
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn build_alternates(
            f: &mut fmt::Formatter,
            ty: ty::Ty,
            edges: &mut HashSet<Edge>,
        ) -> fmt::Result {
            let mut to_visit = HashSet::new();
            for edge in edges.iter() {
                if edge.src == ty {
                    to_visit.insert((edge.atom, edge.dest));
                }
            }
            edges.retain(|edge| edge.src != ty);
            if !to_visit.is_empty() {
                write!(f, ".")?;
            }
            let bracketed = to_visit.len() > 1;
            if bracketed {
                write!(f, "(")?;
            }
            let mut visiting = to_visit.into_iter();
            if let Some((atom, dest)) = visiting.next() {
                write!(f, "{:?}", atom)?;
                build_alternates(f, dest, edges)?;
            }
            for (atom, dest) in visiting {
                write!(f, " + {:?}", atom)?;
                build_alternates(f, dest, edges)?;
            }
            if bracketed {
                write!(f, ")")?;
            }
            Ok(())
        }

        if self.edges.is_empty() {
            write!(f, "{:?}.ϵ", self.src)
        } else {
            write!(f, "{:?}", self.src)?;
            build_alternates(f, self.src, &mut self.edges.clone())?;
            write!(f, ".{:?}", self.dest)
        }
    }
}
