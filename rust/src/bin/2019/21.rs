use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use std::time::Instant;

fn run_springscript(program: Vec<isize>, instructions: &[&str]) -> isize {
    let mut runner = IntcodeRunner::for_program(program);
    instructions.iter().for_each(|instr| {
        instr.chars().for_each(|c| runner.push_input(c as isize));
        runner.push_input('\n' as isize);
    });
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    
    runner.last_output().unwrap()
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/21.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> isize {
    run_springscript(program, &[
        "NOT A J", "NOT C T", "OR T J", "AND D J", "WALK"
    ])
}

fn part2(program: Vec<isize>) -> isize {
    run_springscript(program, &[
        "NOT A J", "NOT B T", "OR T J", "NOT C T", "OR T J", "AND D J", "NOT E T", "NOT T T", "OR H T", "AND T J", "RUN"
    ])
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
