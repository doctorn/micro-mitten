use crate::common::{Idx, IdxVec, IdxVecIterator};

use std::default::Default;
use std::{fmt, mem};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct KeyIdx(usize);

impl Idx for KeyIdx {
    fn new(idx: usize) -> Self {
        KeyIdx(idx)
    }

    fn index(&self) -> usize {
        self.0
    }
}

#[derive(Clone)]
pub struct Map<K, V> {
    keys: IdxVec<KeyIdx, K>,
    values: IdxVec<KeyIdx, V>,
}

impl<K, V> Map<K, V>
where
    K: Eq,
{
    pub fn new() -> Map<K, V> {
        Map {
            keys: IdxVec::new(),
            values: IdxVec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Map<K, V> {
        Map {
            keys: IdxVec::with_capacity(capacity),
            values: IdxVec::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.keys.clear();
        self.values.clear();
    }

    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.keys.values()
    }

    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.values.values()
    }

    #[inline]
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.values.values_mut()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.keys().zip(self.values())
    }

    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.keys.values().zip(self.values.values_mut())
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.keys.len()
    }

    fn key_index_of(&self, k: &K) -> Option<KeyIdx> {
        for (idx, key) in self.keys.iter() {
            if key == k {
                return Some(idx);
            }
        }
        None
    }

    #[inline]
    pub fn get(&self, k: &K) -> Option<&V> {
        self.key_index_of(k).and_then(|idx| self.values.get(idx))
    }

    #[inline]
    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        self.key_index_of(k)
            .and_then(move |idx| self.values.get_mut(idx))
    }

    #[inline]
    pub fn contains_key(&self, k: &K) -> bool {
        self.key_index_of(k).is_some()
    }

    pub fn insert(&mut self, k: K, mut v: V) -> Option<V> {
        if let Some(idx) = self.key_index_of(&k) {
            mem::swap(&mut v, &mut self.values[idx]);
            Some(v)
        } else {
            self.keys.push(k);
            self.values.push(v);
            None
        }
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }
}

impl<K, V> Map<K, V>
where
    K: Eq,
    V: Eq,
{
    pub fn contains_entry(&self, k: &K, v: &V) -> bool {
        self.key_index_of(k)
            .map(|idx| &self.values[idx] == v)
            .unwrap_or(false)
    }
}

impl<K, V> IntoIterator for Map<K, V> {
    type Item = (K, V);
    type IntoIter = MapIterator<K, V>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        MapIterator(self.keys.into_iter(), self.values.into_iter())
    }
}

impl<K, V> PartialEq for Map<K, V>
where
    K: Eq,
    V: Eq,
{
    fn eq(&self, other: &Map<K, V>) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (k, v) in self.iter() {
            if !other.contains_entry(k, v) {
                return false;
            }
        }
        true
    }
}

impl<K, V> Eq for Map<K, V>
where
    K: Eq,
    V: Eq,
{
}

impl<K, V> Default for Map<K, V>
where
    K: Eq,
{
    fn default() -> Map<K, V> {
        Map::new()
    }
}

impl<K, V> fmt::Debug for Map<K, V>
where
    K: Eq + fmt::Debug,
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

pub struct MapIterator<K, V>(IdxVecIterator<KeyIdx, K>, IdxVecIterator<KeyIdx, V>);

impl<K, V> Iterator for MapIterator<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<(K, V)> {
        Some((self.0.next()?.1, self.1.next()?.1))
    }
}
