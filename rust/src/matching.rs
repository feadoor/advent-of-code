use itertools::Itertools;
use std::collections::HashSet;

fn find_naked_single(assignments: &[HashSet<usize>]) -> Option<(usize, usize)> {
    assignments.iter().enumerate()
        .filter_map(|(idx, set)| Some((idx, set.iter().exactly_one().ok().copied()?)))
        .next()
}

fn find_hidden_single(assignments: &[HashSet<usize>]) -> Option<(usize, usize)> {
    (0 .. assignments.len()).filter_map(|val|
        (0 .. assignments.len()).filter(|&idx| assignments[idx].contains(&val)).exactly_one().ok().map(|idx| (idx, val))
    ).next()
}

pub fn find_unique_matching(mut assignments: Vec<HashSet<usize>>) -> Option<Vec<usize>> {
    let mut final_assignments = vec![None; assignments.len()];
    for _ in 0 .. assignments.len() {
        let (idx, val) = find_naked_single(&assignments).or_else(|| find_hidden_single(&assignments))?;
        assignments.iter_mut().for_each(|it| { it.remove(&val); });
        final_assignments[idx] = Some(val);
    }
    final_assignments.into_iter().collect()
}
