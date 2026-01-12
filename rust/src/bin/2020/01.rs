use std::collections::HashSet;
use std::time::Instant;

fn two_sum(values: &[usize], target: usize) -> Option<(usize, usize)> {
    let mut seen = HashSet::new();
    for &value in values {
        if value < target && seen.contains(&(target - value)) { return Some((target - value, value)); }
        else { seen.insert(value); }
    }
    None
}

fn three_sum(values: &[usize], target: usize) -> Option<(usize, usize, usize)> {
    (0 .. values.len())
        .filter(|&idx| values[idx] < target)
        .flat_map(|idx| two_sum(&values[idx + 1..], target - values[idx]).map(|(a, b)| (values[idx], a, b)))
        .next()
}

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2020/01.txt").lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part1(values: &[usize]) -> usize {
    let (a, b) = two_sum(values, 2020).unwrap();
    a * b
}

fn part2(values: &[usize]) -> usize {
    let (a, b, c) = three_sum(values, 2020).unwrap();
    a * b * c
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
