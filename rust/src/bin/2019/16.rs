use itertools::{Itertools, iterate};
use std::iter::repeat_n;
use std::time::Instant;

fn fft(input: &Vec<isize>, offset: usize) -> Vec<isize> {
    let mut partials = vec![0; input.len() + 1];
    for idx in 0 .. input.len() { partials[idx + 1] = partials[idx] + input[idx]; }
    let segment = |l: usize, r: usize| { partials[r.clamp(0, input.len())] - partials[l.clamp(0, input.len())] };
    (1 + offset ..= input.len() + offset).map(|n| {
        let total: isize = (n - 1 ..= input.len() + offset + 3 * n).step_by(n).tuples().map(|(k1, k2, k3, k4)| segment(k1 - offset, k2 - offset) - segment(k3 - offset, k4 - offset)).sum();
        total.abs() % 10
    }).collect()
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/16.txt").trim().chars().map(|c| c.to_digit(10).unwrap() as isize).collect()
}

fn part1(input: &Vec<isize>) -> isize {
    let transformed = iterate(input.to_vec(), |x| fft(x, 0)).nth(100).unwrap();
    transformed.into_iter().take(8).reduce(|a, b| 10 * a + b).unwrap()
}

fn part2(input: &Vec<isize>) -> isize {
    let offset = input.iter().copied().take(7).reduce(|a, b| 10 * a + b).unwrap() as usize;
    let multi_input = repeat_n(input.iter().copied(), 10_000).flatten().skip(offset).collect();
    let transformed = iterate(multi_input, |x| fft(x, offset)).nth(100).unwrap();
    transformed.into_iter().take(8).reduce(|a, b| 10 * a + b).unwrap()
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
