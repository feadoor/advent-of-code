use itertools::Itertools;
use std::time::Instant;

fn is_increasing<I: Iterator<Item = char>>(digits: I) -> bool {
    digits.tuple_windows().all(|(a, b)| a <= b)
}

fn has_duplicate<I: Iterator<Item = char>>(digits: I) -> bool {
    digits.tuple_windows().any(|(a, b)| a == b)
}

fn has_exact_duplicate<I: Iterator<Item = char>>(digits: I) -> bool {
    digits.chunk_by(|&d| d).into_iter().any(|group| group.1.count() == 2)
}

fn parse_input() -> (usize, usize) {
    include_str!("../../../../inputs/2019/04.txt").trim().split("-")
        .map(|s| s.parse().unwrap())
        .collect_tuple().unwrap()
}

fn part1((lo, hi): (usize, usize)) -> usize {
    (lo ..= hi).map(|n| n.to_string())
        .filter(|n| is_increasing(n.chars()) && has_duplicate(n.chars()))
        .count()
}

fn part2((lo, hi): (usize, usize)) -> usize {
    (lo ..= hi).map(|n| n.to_string())
        .filter(|n| is_increasing(n.chars()) && has_exact_duplicate(n.chars()))
        .count()
}

fn main() {
    let start_time = Instant::now();
    let range = parse_input();
    let part1_ans = part1(range);
    let part2_ans = part2(range);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
