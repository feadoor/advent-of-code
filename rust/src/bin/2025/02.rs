use itertools::{Itertools, iterate};
use std::time::Instant;

fn count_digits(n: usize) -> usize {
    iterate(n, |x| x / 10).take_while(|&x| x > 0).count()
}

fn repeat(n: usize, count: usize) -> usize {
    let multiplier = 10usize.pow(count_digits(n) as u32);
    iterate(n, |&x| multiplier * x + n).nth(count - 1).unwrap()
}

fn invalid_ids(repeats: usize, lo: usize, hi: usize) -> impl Iterator<Item = usize> {
    let (lo_digits, hi_digits) = (count_digits(lo), count_digits(hi));
    let (min_invalid_digits, max_invalid_digits) = ((lo_digits + repeats - 1) / repeats, hi_digits / repeats);

    let min_modulus = 10usize.pow(min_invalid_digits as u32);
    let min_invalid = if repeats * max_invalid_digits == lo_digits {
        let first_digits = lo / (min_modulus.pow(repeats as u32 - 1));
        if repeat(first_digits, repeats) >= lo { first_digits } else { first_digits + 1 }
    } else {
        min_modulus / 10
    };

    let max_modulus = 10usize.pow(max_invalid_digits as u32);
    let max_invalid = if repeats * max_invalid_digits == hi_digits {
        let first_digits = hi / (max_modulus.pow(repeats as u32 - 1));
        if repeat(first_digits, repeats) <= hi { first_digits } else { first_digits - 1 }
    } else {
        max_modulus - 1
    };

    (min_invalid ..= max_invalid).map(move |n| repeat(n, repeats))
}

fn all_invalid_ids((lo, hi): (usize, usize)) -> impl Iterator<Item = usize> {
    (2 ..= count_digits(hi)).flat_map(move |repeats| invalid_ids(repeats, lo, hi)).unique()
}

fn parse_input() -> Vec<(usize, usize)> {
    include_str!("../../../../inputs/2025/02.txt").trim().split(",").map(|s| {
        s.split("-").map(|n| n.parse().unwrap()).collect_tuple().unwrap()
    }).collect()
}

fn part1(ranges: &[(usize, usize)]) -> usize {
    ranges.iter().flat_map(|&(lo, hi)| invalid_ids(2, lo, hi)).sum()
}

fn part2(ranges: &[(usize, usize)]) -> usize {
    ranges.iter().copied().flat_map(all_invalid_ids).sum()
}

fn main() {
    let start_time = Instant::now();
    let ranges = parse_input();
    let part1_ans = part1(&ranges);
    let part2_ans = part2(&ranges);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
