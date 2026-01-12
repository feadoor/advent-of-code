use std::time::Instant;

fn parse_input() -> Vec<isize> {
    let lines = include_str!("../../../../inputs/2017/05.txt").lines();
    lines.map(|l| l.parse().unwrap()).collect()
}

fn part1(mut program: Vec<isize>) -> usize {
    let (mut idx, mut steps) = (0, 0);
    while idx >= 0 && idx < program.len() as isize {
        program[idx as usize] += 1;
        idx += program[idx as usize] - 1;
        steps += 1;
    }
    steps
}

fn part2(mut program: Vec<isize>) -> usize {
    let (mut idx, mut steps) = (0, 0);
    while idx >= 0 && idx < program.len() as isize {
        let offset = program[idx as usize];
        program[idx as usize] += if offset < 3 { 1 } else { -1 };
        idx += offset;
        steps += 1;
    }
    steps
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(input.clone());
    let part2_ans = part2(input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
