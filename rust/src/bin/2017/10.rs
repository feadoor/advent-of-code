use advent_of_code::knot_hash::{compute_hash, twist};
use itertools::Itertools;
use std::time::Instant;

fn parse_input_as_ints() -> Vec<usize> {
    include_str!("../../../../inputs/2017/10.txt").trim().split(",").map(|s| s.parse().unwrap()).collect()
}

fn parse_input_as_bytes() -> Vec<usize> {
    include_str!("../../../../inputs/2017/10.txt").trim().bytes().map(|b| b as usize).collect()
}

fn part1() -> usize {
    let mut values = (0 ..= 255).collect_vec();
    let lengths = parse_input_as_ints();
    twist(&mut values, &lengths, 1);
    (values[0] as usize) * (values[1] as usize)
}

fn part2() -> String {
    let dense_hash = compute_hash(&parse_input_as_bytes());
    dense_hash.into_iter().map(|x| format!("{:x}", x)).join("")
}

fn main() {
    let start_time = Instant::now();
    let part1_ans = part1();
    let part2_ans = part2();
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
