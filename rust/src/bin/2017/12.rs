use advent_of_code::union_find::UnionFind;
use itertools::Itertools;
use std::time::Instant;

fn find_groups(relations: &[(usize, Vec<usize>)]) -> UnionFind {
    let mut groups = UnionFind::new(relations.len());
    for (src, dsts) in relations {
        for dst in dsts {
            if src < dst {
                groups.merge(*src, *dst);
            }
        }
    }
    groups
}

fn parse_input() -> Vec<(usize, Vec<usize>)> {
    include_str!("../../../../inputs/2017/12.txt").lines()
        .map(|line| line.split(" <-> ").map(|s| s.to_owned()).collect_tuple().unwrap())
        .map(|(src, dsts)| {
            (src.parse().unwrap(), dsts.split(", ").map(|dst| dst.parse().unwrap()).collect())
        }).collect()
}

fn main() {
    let start_time = Instant::now();
    let relations = parse_input();
    let mut groups = find_groups(&relations);
    let part1_ans = groups.size_of_set_containing(0);
    let part2_ans = groups.len();
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
