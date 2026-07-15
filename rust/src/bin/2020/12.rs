use advent_of_code::grid::{Direction, make_long_move};
use std::time::Instant;

enum Command {
    N(isize),
    S(isize),
    E(isize),
    W(isize),
    L(isize),
    R(isize),
    F(isize),
}

impl Command {
    fn from_str(s: &str) -> Self {
        let (c, amt) = s.split_at(1);
        let amt = amt.parse().unwrap();
        match c {
            "N" => Self::N(amt),
            "S" => Self::S(amt),
            "E" => Self::E(amt),
            "W" => Self::W(amt),
            "L" => Self::L(amt),
            "R" => Self::R(amt),
            "F" => Self::F(amt),
            _ => panic!("Unknown command {}", c),
        }
    }
}

fn parse_input() -> Vec<Command> {
    include_str!("../../../../inputs/2020/12.txt").lines().map(Command::from_str).collect()
}

fn part1(commands: &[Command]) -> usize {
    let (mut pos, mut dir) = ((0isize, 0isize), Direction::Right);
    for command in commands { match command {
        &Command::N(amt) => pos = make_long_move(pos, amt, Direction::Up),
        &Command::S(amt) => pos = make_long_move(pos, amt, Direction::Down),
        &Command::E(amt) => pos = make_long_move(pos, amt, Direction::Right),
        &Command::W(amt) => pos = make_long_move(pos, amt, Direction::Left),
        &Command::L(amt) => for _ in 0 .. amt / 90 { dir = dir.turn_left() },
        &Command::R(amt) => for _ in 0 .. amt / 90 { dir = dir.turn_right() },
        &Command::F(amt) => pos = make_long_move(pos, amt, dir),
    }}
    pos.0.unsigned_abs() + pos.1.unsigned_abs()
}

fn part2(commands: &[Command]) -> usize {
    let (mut pos, mut waypoint) = ((0isize, 0isize), (-1isize, 10isize));
    for command in commands { match command {
        &Command::N(amt) => waypoint = make_long_move(waypoint, amt, Direction::Up),
        &Command::S(amt) => waypoint = make_long_move(waypoint, amt, Direction::Down),
        &Command::E(amt) => waypoint = make_long_move(waypoint, amt, Direction::Right),
        &Command::W(amt) => waypoint = make_long_move(waypoint, amt, Direction::Left),
        &Command::L(amt) => for _ in 0 .. amt / 90 { waypoint = (-waypoint.1, waypoint.0) },
        &Command::R(amt) => for _ in 0 .. amt / 90 { waypoint = (waypoint.1, -waypoint.0) },
        &Command::F(amt) => pos = (pos.0 + amt * waypoint.0, pos.1 + amt * waypoint.1),
    }}
    pos.0.unsigned_abs() + pos.1.unsigned_abs()
}

fn main() {
    let start_time = Instant::now();
    let commands = parse_input();
    let part1_ans = part1(&commands);
    let part2_ans = part2(&commands);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
