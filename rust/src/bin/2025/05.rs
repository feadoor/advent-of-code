use itertools::Itertools;
use std::cmp::max;
use std::time::Instant;

fn parse_input() -> (Vec<(usize, usize)>, Vec<usize>) {
    let mut lines = include_str!("../../../../inputs/2025/05.txt").lines();
    let ranges = lines.by_ref().take_while(|&line| !line.is_empty()).map(|line| 
            line.split("-").map(|s| s.parse().unwrap()).collect_tuple().unwrap()
    ).collect();
    let ids = lines.map(|line| line.parse().unwrap()).collect();
    (ranges, ids)
}

fn part1(ranges: &[(usize, usize)], ids: &[usize]) -> usize {
    ids.iter().filter(|&&id| ranges.iter().any(|&(lo, hi)| lo <= id && id <= hi)).count()
}

fn part2(ranges: &[(usize, usize)]) -> usize {
    ranges.iter().sorted().fold((0, 0), |(count, curr_hi), &(lo, hi)|
        if lo <= curr_hi { (count + hi.saturating_sub(curr_hi), max(curr_hi, hi)) }
        else { (count + hi - lo + 1, hi) }
    ).0
}

fn main() {
    let start_time = Instant::now();
    let (ranges, ids) = parse_input();
    let part1_ans = part1(&ranges, &ids);
    let part2_ans = part2(&ranges);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
