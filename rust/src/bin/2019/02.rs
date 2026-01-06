use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::iproduct;
use std::time::Instant;

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/02.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(mut program: Vec<isize>) -> isize {
    program[1] = 12; program[2] = 2;
    let mut runner = IntcodeRunner::for_program(program);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.get(0)
}

fn part2(program: Vec<isize>) -> isize {
    iproduct!(0 .. 100, 0 .. 100).find(|&(noun, verb)| {
        let mut program = program.clone();
        program[1] = noun; program[2] = verb;
        let mut runner = IntcodeRunner::for_program(program);
        assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
        runner.get(0) == 19690720
    }).map(|(noun, verb)| 100 * noun + verb).unwrap()
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
