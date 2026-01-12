use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::iter::successors;
use std::time::Instant;

fn parse_input_forwards() -> HashMap<&'static str, Vec<&'static str>> {
    include_str!("../../../../inputs/2019/06.txt").lines()
        .map(|line| line.split_once(")").unwrap())
        .into_group_map()
}

fn parse_input_backwards() -> HashMap<&'static str, &'static str> {
    include_str!("../../../../inputs/2019/06.txt").lines()
        .map(|line| line.split_once(")").unwrap())
        .map(|(parent, child)| (child, parent))
        .collect()
}

fn part1(orbits: &HashMap<&str, Vec<&str>>) -> usize {
    let (mut result, mut stack) = (0, vec![("COM", 0)]);
    while let Some((object, depth)) = stack.pop() {
        result += depth;
        if let Some(children) = orbits.get(object) {
            children.iter().for_each(|&child| stack.push((child, depth + 1)));
        }
    }
    result
}

fn part2(ancestors: &HashMap<&str, &str>) -> usize {
    let you_ancestors: HashSet<_> = successors(Some("YOU"), |&s| ancestors.get(s).copied()).skip(1).collect();
    let san_ancestors: HashSet<_> = successors(Some("SAN"), |&s| ancestors.get(s).copied()).skip(1).collect();
    let you_unique = you_ancestors.iter().filter(|&it| !san_ancestors.contains(it)).count();
    let san_unique = san_ancestors.iter().filter(|&it| !you_ancestors.contains(it)).count();
    you_unique + san_unique
}

fn main() {
    let start_time = Instant::now();
    let part1_ans = part1(&parse_input_forwards());
    let part2_ans = part2(&parse_input_backwards());
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
