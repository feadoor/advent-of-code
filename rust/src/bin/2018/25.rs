use advent_of_code::union_find::UnionFind;
use itertools::Itertools;
use std::time::Instant;

fn manhattan(p1: (isize, isize, isize, isize), p2: (isize, isize, isize, isize)) -> usize {
    p1.0.abs_diff(p2.0) + p1.1.abs_diff(p2.1) + p1.2.abs_diff(p2.2) + p1.3.abs_diff(p2.3)
}

fn parse_input() -> Vec<(isize, isize, isize, isize)> {
    include_str!("../../../../inputs/2018/25.txt").lines()
        .map(|line| line.split(",").map(|s| s.parse().unwrap()).collect_tuple().unwrap())
        .collect()
}

fn part1(points: &[(isize, isize, isize, isize)]) -> usize {
    let mut union_find = UnionFind::new(points.len());
    for idx in 0 .. points.len() {
        for jdx in 0 .. points.len() {
            if manhattan(points[idx], points[jdx]) <= 3 {
                union_find.merge(idx, jdx);
            }
        }
    }
    union_find.len()
}

fn main() {
    let start_time = Instant::now();
    let points = parse_input();
    let part1_ans = part1(&points);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", "Congratulations!");
    println!("Elapsed: {:?}", elapsed_time);
}
