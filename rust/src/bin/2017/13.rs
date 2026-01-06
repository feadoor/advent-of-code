use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> Vec<(usize, usize)> {
    include_str!("../../../../inputs/2017/13.txt").lines()
        .map(|line| line.split(": ").map(|s| s.parse().unwrap()).collect_tuple().unwrap())
        .collect()
}

fn part1(scanners: &[(usize, usize)]) -> usize {
    scanners.iter().copied().filter_map(|(depth, range)| {
        if depth % (2 * (range - 1)) == 0 { Some(depth * range) } else { None }
    }).sum()
}

fn part2(scanners: &[(usize, usize)]) -> usize {
    (0..)
        .find(|t| scanners.iter().copied().all(|(depth, range)| (t + depth) % (2 * (range - 1)) != 0))
        .unwrap()
}

fn main() {
    let start_time = Instant::now();
    let scanners = parse_input();
    let part1_ans = part1(&scanners);
    let part2_ans = part2(&scanners);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
