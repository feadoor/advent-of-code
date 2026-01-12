use itertools::Itertools;
use std::cmp::{max, min};
use std::time::Instant;

struct RangeData {
    sum: usize,
    min: usize,
    max: usize,
}

impl RangeData {
    fn singleton(value: usize) -> Self {
        Self { sum: value, min: value, max: value }
    }

    fn update(&mut self, value: usize) {
        self.sum += value;
        self.min = min(self.min, value);
        self.max = max(self.max, value);
    }
}

fn pairwise_sums(vals: &[usize], start: usize, end: usize) -> impl Iterator<Item = usize> {
    (start .. end).flat_map(move |l| (l + 1 .. end).map(move |r| vals[l] + vals[r]))
}

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2020/09.txt").lines()
        .map(|line| line.parse().unwrap())
        .collect()
}

fn part1(values: &[usize]) -> usize {
    *values.iter().enumerate().skip(25)
        .find(|&(idx, val)| !pairwise_sums(values, idx - 25, idx).contains(val))
        .unwrap().1
}

fn part2(values: &[usize], target: usize) -> usize {
    let mut ranges: Vec<RangeData> = Vec::new();
    for &value in values.iter() {
        for range in ranges.iter_mut() {
            range.update(value);
            if range.sum == target { return range.min + range.max; }
        }
        ranges.push(RangeData::singleton(value));
    }
    panic!("No valid range found");
}

fn main() {
    let start_time = Instant::now();
    let values = parse_input();
    let part1_ans = part1(&values);
    let part2_ans = part2(&values, part1_ans);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
