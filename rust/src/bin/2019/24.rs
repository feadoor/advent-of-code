use advent_of_code::grid::four_neighbours_hw;
use advent_of_code::more_itertools::FirstDuplicateExt;
use itertools::{Itertools, iproduct, iterate};
use std::collections::HashSet;
use std::hash::Hash;
use std::time::Instant;

fn neighbours(&(y, x): &(usize, usize)) -> Vec<(usize, usize)> {
    four_neighbours_hw((y, x), 5, 5)
}

fn deep_neighbours(&(y, x, d): &(usize, usize, isize)) -> Vec<(usize, usize, isize)> {
    let mut neighbours = four_neighbours_hw((y, x), 5, 5).into_iter().filter(|&nb| nb != (2, 2)).map(|(b, a)| (b, a, d)).collect_vec();
    if (y, x) == (2, 1) { neighbours.extend((0 .. 5).map(|b| (b, 0, d + 1))); }
    if (y, x) == (2, 3) { neighbours.extend((0 .. 5).map(|b| (b, 4, d + 1))); }
    if (y, x) == (1, 2) { neighbours.extend((0 .. 5).map(|a| (0, a, d + 1))); }
    if (y, x) == (3, 2) { neighbours.extend((0 .. 5).map(|a| (4, a, d + 1))); }
    if x == 0 { neighbours.push((2, 1, d - 1)); }
    if x == 4 { neighbours.push((2, 3, d - 1)); }
    if y == 0 { neighbours.push((1, 2, d - 1)); }
    if y == 4 { neighbours.push((3, 2, d - 1)); }
    neighbours
}

fn lives<T: Eq + Hash>(bugs: &HashSet<T>, pos: &T, cnt: usize) -> bool {
    cnt == 1 || (cnt == 2 && !bugs.contains(pos))
}

fn tick<T: Eq + Hash + Clone, F: Fn(&T) -> Vec<T>>(bugs: &HashSet<T>, nbrs: F) -> HashSet<T> {
    let neighbour_counts = bugs.iter().flat_map(|bug| nbrs(bug)).counts();
    neighbour_counts.into_iter().filter_map(|(pos, cnt)| lives(bugs, &pos, cnt).then_some(pos)).collect()
}

fn biodiversity(bugs: &HashSet<(usize, usize)>) -> usize {
    iproduct!(0 .. 5, 0 .. 5).zip(iterate(1, |x| 2 * x))
        .filter_map(|((y, x), v)| bugs.contains(&(y, x)).then_some(v))
        .sum()
}

fn parse_input() -> HashSet<(usize, usize)> {
    include_str!("../../../../inputs/2019/24.txt").lines().enumerate().flat_map(|(y, line)| 
        line.chars().enumerate().filter_map(move |(x, c)| (c == '#').then_some((y, x)))
    ).collect()
}

fn part1(starting_bugs: &HashSet<(usize, usize)>) -> usize {
    iterate(starting_bugs.clone(), |bugs| tick(bugs, neighbours))
        .map(|bugs| biodiversity(&bugs))
        .first_duplicate().unwrap()
}

fn part2(starting_bugs: &HashSet<(usize, usize)>) -> usize {
    let deep_bugs = starting_bugs.iter().map(|&(y, x)| (y, x, 0)).collect();
    iterate(deep_bugs, |bugs| tick(bugs, deep_neighbours)).nth(200).unwrap().len()
}

fn main() {
    let start_time = Instant::now();
    let bugs = parse_input();
    let part1_ans = part1(&bugs);
    let part2_ans = part2(&bugs);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
