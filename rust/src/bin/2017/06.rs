use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

fn find_first_repeat(mut data: Vec<usize>) -> (usize, usize) {
    let mut seen = HashMap::new();
    let mut steps = 0;
    let len = data.len();

    while !seen.contains_key(&data) {
        seen.insert(data.clone(), steps);
        let max_value = *data.iter().max().unwrap();
        let start_idx = data.iter().find_position(|&&v| v == max_value).unwrap().0;
        data[start_idx] = 0;
        for idx in (start_idx + 1 ..).take(max_value).map(|idx| idx % len) {
            data[idx] += 1;
        }
        steps += 1;
    }

    (seen[&data], steps)
}

fn parse_input() -> Vec<usize> {
    let input = include_str!("../../../../inputs/2017/06.txt").trim();
    input.split_ascii_whitespace().map(|n| n.parse().unwrap()).collect()
}

fn both_parts(data: Vec<usize>) -> (usize, usize) {
    let repeated_indices = find_first_repeat(data);
    (repeated_indices.1, repeated_indices.1 - repeated_indices.0)
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let result = both_parts(input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", result.0);
    println!("Part 2: {}", result.1);
    println!("Elapsed: {:?}", elapsed_time);
}
