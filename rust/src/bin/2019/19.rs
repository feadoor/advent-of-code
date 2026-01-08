use advent_of_code::intcode::{IntcodeInterrupt, IntcodeRunner};
use std::time::Instant;

fn is_beam(program: &[isize], y: isize, x: isize) -> bool {
    let mut runner = IntcodeRunner::for_program(program.to_vec());
    runner.push_input(x);
    runner.push_input(y);
    assert!(matches!(runner.run(), IntcodeInterrupt::Halt));
    runner.pop_output() == Some(1)
}

fn get_count(program: &[isize], height: isize, width: isize) -> usize {
    let (mut start_x, mut count) = (0, 0);
    for y in 0 .. height {
        start_x = (start_x .. width).find(|&x| is_beam(program, y, x)).unwrap_or(start_x);
        count += (start_x .. width).take_while(|&x| is_beam(program, y, x)).count();
    }
    count
}

fn get_first_box(program: &[isize], dy: isize, dx: isize) -> (isize, isize) {
    let mut x = 0;
    for y in dy - 1 .. {
        x = (x..).find(|&x| is_beam(program, y, x)).unwrap();
        if !is_beam(program, y, x + dx - 1) { continue; }
        if is_beam(program, y - dy + 1, x + dx - 1) { return (y - dy + 1, x); }
    }
    unreachable!()
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/19.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: &[isize]) -> usize {
    get_count(program, 50, 50)
}

fn part2(program: &[isize]) -> isize {
    let (y, x) = get_first_box(program, 100, 100);
    10_000 * x + y
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let part1_ans = part1(&program);
    let part2_ans = part2(&program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
