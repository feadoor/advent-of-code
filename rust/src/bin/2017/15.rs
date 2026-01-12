use itertools::{Itertools, iterate};
use std::time::Instant;

const MUL_A: usize = 16_807;
const MUL_B: usize = 48_271;
const DIV_A: usize = 4;
const DIV_B: usize = 8;

fn generator(start: usize, multiplier: usize, divisor_check: usize) -> impl Iterator<Item = usize> {
    iterate(start, move |x| (x * multiplier) % 2147483647)
        .skip(1)
        .filter(move |&x| x % divisor_check == 0)
}

fn parse_input() -> (usize, usize) {
    include_str!("../../../../inputs/2017/15.txt").lines()
        .map(|line| line.chars().filter(|c| c.is_numeric()).collect::<String>().parse().unwrap())
        .collect_tuple().unwrap()
}

fn part1(start_a: usize, start_b: usize) -> usize {
    generator(start_a, MUL_A, 1)
        .zip(generator(start_b, MUL_B, 1))
        .take(40_000_000)
        .filter(|&(a, b)| a as u16 == b as u16)
        .count()
}

fn part2(start_a: usize, start_b: usize) -> usize {
    generator(start_a, MUL_A, DIV_A)
        .zip(generator(start_b, MUL_B, DIV_B))
        .take(5_000_000)
        .filter(|&(a, b)| a as u16 == b as u16)
        .count()
}

fn main() {
    let start_time = Instant::now();
    let (start_a, start_b) = parse_input();
    let part1_ans = part1(start_a, start_b);
    let part2_ans = part2(start_a, start_b);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
