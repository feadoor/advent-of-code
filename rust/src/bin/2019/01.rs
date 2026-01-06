use itertools::iterate;
use std::time::Instant;

fn fuel(x: usize) -> usize {
    (x / 3).saturating_sub(2)
}

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2019/01.txt").lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part1(values: &[usize]) -> usize {
    values.iter().map(|&v| fuel(v)).sum()
}

fn part2(values: &[usize]) -> usize {
    values.iter().flat_map(|&v| iterate(fuel(v), |&x| fuel(x)).take_while(|&x| x > 0)).sum()
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
