use advent_of_code::dag::{Dag, lexmin_toposort};
use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

fn paths_between(start: &str, end: &str, connections: &Dag<&'static str>, topo: &[&&str]) -> usize {
    let mut result = HashMap::from([(end, 1)]);
    for &src in topo.iter().rev().skip_while(|&&&s| s != end).take_while_inclusive(|&&&s| s != start).skip(1) {
        result.insert(src, connections[src].iter().map(|dst| result.get(dst).unwrap_or(&0)).sum());
    }
    result[start]
}

fn parse_input() -> Dag<&'static str> {
    include_str!("../../../../inputs/2025/11.txt").lines().map(|line| {
        let (id, others) = line.split_once(": ").unwrap();
        (id, others.split(" ").collect())
    }).collect()
}

fn part1(connections: &Dag<&'static str>, topo: &[&&str]) -> usize {
    paths_between("you", "out", connections, topo)
}

fn part2(connections: &Dag<&'static str>, topo: &[&&str]) -> usize {
    let dac_first = topo.iter().find(|&&&s| s == "dac" || s == "fft").unwrap() == &&"dac";
    let (via1, via2) = if dac_first { ("dac", "fft") } else { ("fft", "dac") };
    ["svr", via1, via2, "out"].into_iter().tuple_windows().map(|(src, dst)| paths_between(src, dst, connections, topo)).product()
}

fn main() {
    let start_time = Instant::now();
    let connections = parse_input();
    let topo = lexmin_toposort(&connections);
    let part1_ans = part1(&connections, &topo);
    let part2_ans = part2(&connections, &topo);
    let elapsed_time = start_time.elapsed();
    
    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
