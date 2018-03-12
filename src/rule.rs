use std::collections::{BTreeMap, HashMap};
use std::hash::{BuildHasher, Hash};
use std::cmp::Ord;

pub trait Rule<P> {
    fn map(&self, p: &P) -> bool;
}

impl<F, P> Rule<P> for F
where
    F: Fn(&P) -> bool,
{
    fn map(&self, p: &P) -> bool {
        self(p)
    }
}

pub trait PartialRule<P> {
    fn try_map(&self, p: &P) -> Option<bool>;
}

impl<F, P> PartialRule<P> for F
where
    F: Fn(&P) -> Option<bool>,
{
    fn try_map(&self, p: &P) -> Option<bool> {
        self(p)
    }
}

impl<P, S> PartialRule<P> for HashMap<P, bool, S>
where
    P: Eq + Hash,
    S: BuildHasher,
{
    fn try_map(&self, p: &P) -> Option<bool> {
        self.get(p).cloned()
    }
}

impl<P> PartialRule<P> for BTreeMap<P, bool>
where
    P: Eq + Ord,
{
    fn try_map(&self, p: &P) -> Option<bool> {
        self.get(p).cloned()
    }
}
