use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::Itertools;
use std::io;
use std::io::BufRead;

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/25.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn play_game(program: Vec<isize>) {
    let mut runner = IntcodeRunner::for_program(program);
    let mut input_lines = io::stdin().lock().lines();
    loop {
        let interrupt = runner.run();
        println!("{}", runner.outputs.drain(0..).map(|c| c as u8 as char).join(""));
        match interrupt {
            IntcodeInterrupt::Halt => break,
            IntcodeInterrupt::NeedsInput => {
                let input_line = input_lines.next().unwrap().unwrap();
                runner.push_inputs(input_line.chars().map(|c| c as isize));
                runner.push_input('\n' as isize);
            }
        }
    }
}

fn main() {
    let program = parse_input();
    play_game(program);
}
