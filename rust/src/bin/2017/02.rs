use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> Vec<Vec<usize>> {
    let lines = include_str!("../../../../inputs/2017/02.txt").lines();
    lines.map(|l| l.split_ascii_whitespace().map(|n| n.parse::<usize>().unwrap()).collect()).collect()
}

fn part1(values: &Vec<Vec<usize>>) -> usize {
    values.iter().map(|vs| vs.iter().max().unwrap() - vs.iter().min().unwrap()).sum()
}

fn part2(values: &Vec<Vec<usize>>) -> usize {
    values.iter().map(|vs| 
        vs.iter().tuple_combinations()
            .filter_map(|(&a, &b)| if a % b == 0 { Some(a / b) } else if b % a == 0 { Some(b / a) } else { None })
            .next().unwrap()
    ).sum()
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(&input);
    let part2_ans = part2(&input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
