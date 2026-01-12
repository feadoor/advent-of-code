use itertools::{Itertools, iterate};
use std::time::Instant;

fn get_seat_id(s: &str) -> usize {
    let (row_s, col_s) = s.split_at(7);
    let row: usize = row_s.bytes().rev().zip(iterate(1, |x| 2 * x)).filter_map(|(c, v)| (c == b'B').then_some(v)).sum();
    let col: usize = col_s.bytes().rev().zip(iterate(1, |x| 2 * x)).filter_map(|(c, v)| (c == b'R').then_some(v)).sum();
    8 * row + col
}

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2020/05.txt").lines().map(get_seat_id).collect()
}

fn part1(seats: &[usize]) -> usize {
    *seats.iter().max().unwrap()
}

fn part2(seats: &[usize]) -> usize {
    seats.iter().copied().sorted().tuple_windows().find_map(|(a, b)| (b == a + 2).then_some(a + 1)).unwrap()
}

fn main() {
    let start_time = Instant::now();
    let seats = parse_input();
    let part1_ans = part1(&seats);
    let part2_ans = part2(&seats);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
