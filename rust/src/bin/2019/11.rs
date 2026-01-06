use advent_of_code::grid::{Direction, make_move};
use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::Itertools;
use std::collections::HashSet;
use std::time::Instant;

fn tick(robot: &mut IntcodeRunner, pos: &mut (isize, isize), direction: &mut Direction, field: &mut HashSet<(isize, isize)>) {
    robot.push_input(if field.contains(pos) { 1 } else { 0 });
    robot.run();
    let (colour, rotation) = (robot.pop_output().unwrap(), robot.pop_output().unwrap());
    match colour { 0 => field.remove(pos), 1 => field.insert(*pos), _ => panic!("Unknown colour code {}", colour) };
    match rotation { 0 => *direction = direction.turn_left(), 1 => *direction = direction.turn_right(), _ => panic!("Unknown rotation code {}", rotation) };
    *pos = make_move(*pos, *direction);
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/11.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> usize {
    let mut robot = IntcodeRunner::for_program(program);
    let mut field = HashSet::new();
    let (mut pos, mut direction) = ((0, 0), Direction::Up);
    let mut painted_cells = HashSet::new();
    while matches!(robot.run(), IntcodeInterrupt::NeedsInput) {
        painted_cells.insert(pos);
        tick(&mut robot, &mut pos, &mut direction, &mut field);
    }
    painted_cells.len()
}

fn part2(program: Vec<isize>) -> String {
    let mut robot = IntcodeRunner::for_program(program);
    let mut field = HashSet::from([(0, 0)]);
    let (mut pos, mut direction) = ((0, 0), Direction::Up);
    while matches!(robot.run(), IntcodeInterrupt::NeedsInput) {
        tick(&mut robot, &mut pos, &mut direction, &mut field);
    }

    let (min_y, max_y) = field.iter().map(|(y, _x)| *y).minmax().into_option().unwrap();
    let (min_x, max_x) = field.iter().map(|(_y, x)| *x).minmax().into_option().unwrap();
    (min_y ..= max_y).map(|y| (min_x ..= max_x).map(|x| if field.contains(&(y, x)) { '#' } else { ' ' }).join("")).join("\n")
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let part1_ans = part1(program.clone());
    let part2_ans = part2(program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: \n\n{}\n", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
