use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use std::time::Instant;

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/05.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> isize {
    let mut runner = IntcodeRunner::for_program(program);
    runner.push_input(1);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.last_output().unwrap()
}

fn part2(program: Vec<isize>) -> isize {
    let mut runner = IntcodeRunner::for_program(program);
    runner.push_input(5);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.last_output().unwrap()
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
