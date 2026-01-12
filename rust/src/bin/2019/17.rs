use advent_of_code::grid::{Direction, four_neighbours};
use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use itertools::iproduct;
use std::mem::take;
use std::time::Instant;

fn parse_grid<I: Iterator<Item = isize>>(outputs: I) -> Vec<Vec<bool>> {
    let (mut result, mut row, mut robot_location, mut robot_direction) = (Vec::new(), Vec::new(), None, None);
    outputs.for_each(|code| match code {
        10 => if row.len() > 0 { result.push(take(&mut row)) },
        35 => row.push(true),
        46 => row.push(false),
        94 | 118 | 60 | 62 => {
            robot_location = Some((result.len(), row.len()));
            robot_direction = Some(Direction::from_arrow_char(code as u8 as char));
            row.push(true);
        },
        _ => panic!("Unexpected code {}", code),
    });
    result
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/17.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: Vec<isize>) -> usize {
    let mut runner = IntcodeRunner::for_program(program);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    let grid = parse_grid(runner.outputs.into_iter());
    let intersections = iproduct!(0 .. grid.len(), 0 .. grid[0].len()).filter(|&(y, x)| 
        grid[y][x] && four_neighbours((y, x), &grid).into_iter().filter(|&(b, a)| grid[b][a]).count() == 4
    );
    intersections.map(|(y, x)| y * x).sum()
}

fn part2(mut program: Vec<isize>) -> isize {
    // Inputs found by hand after visually examining the map from Part 1
    const INPUTS: &'static str = "A,B,A,C,B,C,A,B,A,C\n\
                                  R,10,L,8,R,10,R,4\n\
                                  L,6,L,6,R,10\n\
                                  L,6,R,12,R,12,R,10\n\
                                  n\n";
    program[0] = 2; let mut runner = IntcodeRunner::for_program(program);
    INPUTS.chars().for_each(|c| runner.push_input(c as isize));
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
