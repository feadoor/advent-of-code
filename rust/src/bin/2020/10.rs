use itertools::Itertools;
use std::iter::{once, repeat_n};
use std::time::Instant;

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2020/10.txt").lines()
        .map(|line| line.parse().unwrap())
        .sorted()
        .collect()
}

fn part1(values: &[usize]) -> usize {
    let effective_values = once(0).chain(values.iter().copied()).chain(once(*values.last().unwrap() + 3));
    let (ones, threes) = effective_values.tuple_windows().fold((0, 0), |(ones, threes), (x, y)| match y - x {
        1 => (ones + 1, threes),
        3 => (ones, threes + 1),
        _ => (ones, threes)
    });
    ones * threes
}

fn part2(values: &[usize]) -> usize {
    let mut result = once(1).chain(repeat_n(0, *values.last().unwrap())).collect_vec();
    for &value in values {
        result[value] = [1, 2, 3].into_iter().filter_map(|d| value.checked_sub(d).map(|v| result[v])).sum();
    }
    *result.last().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let values = parse_input();
    let part1_ans = part1(&values);
    let part2_ans = part2(&values);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
