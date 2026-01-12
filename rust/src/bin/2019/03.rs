use advent_of_code::grid::{Direction, make_move};
use itertools::Itertools;
use std::collections::HashMap;
use std::iter::repeat_n;
use std::time::Instant;

fn indexed_positions(wire: &[(Direction, usize)]) -> HashMap<(isize, isize), usize> {
    let steps = wire.iter().flat_map(|&(dir, len)| repeat_n(dir, len));
    let indexed_positions = steps.scan((0, 0), |pos, dir| {
        *pos = make_move(*pos, dir); Some(*pos)
    }).zip(1..);
    indexed_positions.into_grouping_map().min()
}

fn intersections(indexed_positions1: &HashMap<(isize, isize), usize>, indexed_positions2: &HashMap<(isize, isize), usize>) -> Vec<(isize, isize)> {
    let (small, big) = 
    if indexed_positions1.len() < indexed_positions2.len() { (indexed_positions1, indexed_positions2) } 
    else { (indexed_positions2, indexed_positions1) };
    small.keys().copied().filter(|k| big.contains_key(k)).collect()
}

fn manhattan((y, x): (isize, isize), (b, a): (isize, isize)) -> usize {
    y.abs_diff(b) + x.abs_diff(a)
}

fn parse_step(s: &str) -> (Direction, usize) {
    (Direction::from_direction_char(s.chars().next().unwrap()), s[1..].parse().unwrap())
}

fn parse_input() -> (Vec<(Direction, usize)>, Vec<(Direction, usize)>) {
    include_str!("../../../../inputs/2019/03.txt").lines()
        .map(|line| line.split(",").map(parse_step).collect())
        .collect_tuple().unwrap()
}

fn part1(intersections: &[(isize, isize)]) -> usize {
    intersections.iter().map(|&int| manhattan((0, 0), int)).min().unwrap()
}

fn part2(indexed_positions1: &HashMap<(isize, isize), usize>, indexed_positions2: &HashMap<(isize, isize), usize>, intersections: &[(isize, isize)]) -> usize {
    intersections.iter().map(|int| indexed_positions1[int] + indexed_positions2[int]).min().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let (wire1, wire2) = parse_input();
    let (indexed_positions1, indexed_positions2) = (indexed_positions(&wire1), indexed_positions(&wire2));
    let intersections = intersections(&indexed_positions1, &indexed_positions2);
    let part1_ans = part1(&intersections);
    let part2_ans = part2(&indexed_positions1, &indexed_positions2, &intersections);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
