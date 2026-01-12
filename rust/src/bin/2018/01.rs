use std::collections::HashSet;
use std::time::Instant;

fn parse_input() -> Vec<isize> {
    let lines = include_str!("../../../../inputs/2018/01.txt").lines();
    lines.map(|line| line.parse().unwrap()).collect()
}

fn part1(numbers: &[isize]) -> isize {
    numbers.iter().copied().sum()
}

fn part2(numbers: &[isize]) -> isize {
    let (mut seen_values, mut frequency) = (HashSet::new(), 0);
    for value in numbers.iter().copied().cycle() {
        frequency += value;
        if !seen_values.insert(frequency) { return frequency; }
    }

    unreachable!()
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(&input);
    let part2_ans = part2(&input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
