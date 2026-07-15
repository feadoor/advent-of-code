use advent_of_code::math::{chinese_remainder, mod_neg};
use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> (usize, Vec<(usize, usize)>) {
    let (first_line, second_line) = include_str!("../../../../inputs/2020/13.txt").lines().collect_tuple().unwrap();
    (first_line.parse().unwrap(), second_line.split(",").enumerate().filter_map(|(t, s)| s.parse().ok().map(|v| (t, v))).collect())
}

fn part1(delay: usize, bus_ids: &[(usize, usize)]) -> usize {
    let departures = bus_ids.iter().map(|&(_t, id)| (id, id * ((delay + id - 1) / id)));
    departures.min_by_key(|&(_id, time)| time).map(|(id, time)| id * (time - delay)).unwrap()
}

fn part2(bus_ids: &[(usize, usize)]) -> usize {
    bus_ids.iter()
        .map(|&(t, id)| (mod_neg(t, id), id))
        .reduce(|x, y| chinese_remainder(x, y).unwrap())
        .unwrap().0
}

fn main() {
    let start_time = Instant::now();
    let (delay, bus_ids) = parse_input();
    let part1_ans = part1(delay, &bus_ids);
    let part2_ans = part2(&bus_ids);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
