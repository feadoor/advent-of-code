use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::Itertools;
use std::time::Instant;

struct GameState {
    ball_x: Option<isize>,
    paddle_x: Option<isize>,
    score: isize,
}

impl GameState {
    fn new() -> Self {
        Self { ball_x: None, paddle_x: None, score: 0 }
    }

    fn next_input(&self) -> isize {
        match (self.ball_x, self.paddle_x) {
            (Some(b), Some(p)) => (b - p).signum(),
            _ => 0,
        }
    }
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/13.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> usize {
    let mut runner = IntcodeRunner::for_program(program);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.outputs.into_iter().skip(2).step_by(3).filter(|&it| it == 2).count()
}

fn part2(mut program: Vec<isize>) -> isize {
    program[0] = 2; let mut game = IntcodeRunner::for_program(program);
    let mut state = GameState::new();
    loop {
        let interrupt = game.run();
        for (x, y, tile) in game.outputs.drain(0..).tuples() { match (x, y, tile) {
            (x, _, 3) => state.paddle_x = Some(x),
            (x, _, 4) => state.ball_x = Some(x),
            (-1, 0, score) => state.score = score,
            _ => {},
        }}
        match interrupt {
            IntcodeInterrupt::NeedsInput => game.push_input(state.next_input()),
            IntcodeInterrupt::Halt => break
        }
    }
    state.score
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
