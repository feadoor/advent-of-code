use itertools::Itertools;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

pub type Dag<T> = HashMap<T, HashSet<T>>;

pub fn reversed<'a, T>(dag: &'a Dag<T>) -> Dag<&'a T> where T: Hash + Eq {
    dag.iter()
        .flat_map(|(src, dsts)| dsts.iter().map(move |dst| (dst, src)))
        .into_grouping_map()
        .collect()
}

pub fn lexmin_toposort<'a, T>(dag: &'a Dag<T>) -> Vec<&'a T> where T: Hash + Eq + Ord {
    let mut incoming_edges = reversed(dag);
    let mut result = Vec::new();
    let mut available: BinaryHeap<_> = dag.keys().filter(|k| !incoming_edges.contains_key(k)).map(|k| Reverse(k)).collect();

    while let Some(Reverse(src)) = available.pop() {
        result.push(src);
        for dst in dag.get(src).into_iter().flatten() {
            let in_edges = incoming_edges.get_mut(dst).unwrap();
            in_edges.remove(src);
            if in_edges.is_empty() { available.push(Reverse(dst)); }
        }
    }

    result
}
