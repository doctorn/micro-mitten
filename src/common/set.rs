use std::default::Default;
use std::fmt;
use std::iter::FromIterator;

#[derive(Clone, Default)]
pub struct Set<T> {
    raw: Vec<T>,
}

impl<T> Set<T>
where
    T: Eq,
{
    pub fn new() -> Set<T> {
        Set { raw: vec![] }
    }

    pub fn with_capacity(capacity: usize) -> Set<T> {
        Set {
            raw: Vec::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn singleton(t: T) -> Set<T> {
        let mut set = Set::new();
        set.insert(t);
        set
    }

    #[inline]
    pub fn clear(&mut self) {
        self.raw.clear()
    }

    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.raw.iter()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.raw.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    #[inline]
    pub fn contains(&self, t: &T) -> bool {
        self.raw.contains(t)
    }

    pub fn remove(&mut self, t: &T) -> Option<T> {
        for i in 0..self.raw.len() {
            if &self.raw[i] == t {
                return Some(self.raw.remove(i));
            }
        }
        None
    }

    pub fn is_subset(&self, other: &Set<T>) -> bool {
        for t in self.iter() {
            if !other.contains(&t) {
                return false;
            }
        }
        true
    }

    #[inline]
    pub fn is_superset(&self, other: &Set<T>) -> bool {
        other.is_subset(self)
    }

    pub fn insert(&mut self, t: T) {
        if !self.contains(&t) {
            self.raw.push(t);
        }
    }

    pub fn insert_all(&mut self, iter: impl Iterator<Item = T>) {
        for t in iter {
            self.insert(t);
        }
    }
}

impl<T> PartialEq for Set<T>
where
    T: Eq,
{
    fn eq(&self, other: &Set<T>) -> bool {
        self.is_subset(other) && other.is_subset(self)
    }
}

impl<T> Eq for Set<T> where T: Eq {}

impl<T> IntoIterator for Set<T>
where
    T: Eq,
{
    type Item = T;
    type IntoIter = SetIterator<T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        SetIterator(self.raw.into_iter())
    }
}

impl<T> FromIterator<T> for Set<T>
where
    T: Eq,
{
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut set = Set::new();
        for item in iter {
            set.insert(item);
        }
        set
    }
}

impl<T> fmt::Debug for Set<T>
where
    T: Eq + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

pub struct SetIterator<T>(<Vec<T> as IntoIterator>::IntoIter);

impl<T> Iterator for SetIterator<T> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<T> {
        self.0.next()
    }
}
