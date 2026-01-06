use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/07.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> isize {
    (0 .. 5).permutations(5).map(|inputs| {
        let mut amps = (0 .. 5).map(|_| IntcodeRunner::for_program(program.clone())).collect_vec();
        amps.iter_mut().zip(inputs.into_iter()).for_each(|(amp, input)| amp.push_input(input));

        let mut last_output = 0;
        for idx in 0 .. 5 { amps[idx].push_input(last_output); amps[idx].run(); last_output = amps[idx].pop_output().unwrap(); }
        last_output
    }).max().unwrap()
}

fn part2(program: Vec<isize>) -> isize {
    (5 .. 10).permutations(5).map(|inputs| {
        let mut amps = (0 .. 5).map(|_| IntcodeRunner::for_program(program.clone())).collect_vec();
        amps.iter_mut().zip(inputs.into_iter()).for_each(|(amp, input)| amp.push_input(input));
        amps[0].push_input(0);

        loop {
            for idx in 0 .. 5 { while let Some(output) = amps[idx].pop_output() { amps[(idx + 1) % 5].push_input(output); }}
            let interrupts = amps.iter_mut().map(|amp| amp.run()).collect_vec();
            if interrupts.into_iter().all(|int| matches!(int, IntcodeInterrupt::Halt)) { break; }
        }

        amps[4].last_output().unwrap()
    }).max().unwrap()
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
