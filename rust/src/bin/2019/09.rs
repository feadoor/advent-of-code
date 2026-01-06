use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/09.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> isize {
    let mut runner = IntcodeRunner::for_program(program);
    runner.push_input(1);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.outputs.iter().copied().exactly_one().unwrap()
}

fn part2(program: Vec<isize>) -> isize {
    let mut runner = IntcodeRunner::for_program(program);
    runner.push_input(2);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.outputs.iter().copied().exactly_one().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let part1_ans = part1(program.clone());
    let part2_ans = part2(program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
