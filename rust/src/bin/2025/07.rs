use std::collections::HashMap;
use std::time::Instant;

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2025/07.txt")
}

fn both_parts(grid: &str) -> (usize, usize) {
    let (mut splits, mut dp) = (0, HashMap::new());
    grid.lines().flat_map(|line| line.chars().enumerate()).for_each(|(col, c)| match c {
        'S' => { dp.insert(col, 1); },
        '^' => if let Some(count) = dp.remove(&col) {
            splits += 1;
            *dp.entry(col - 1).or_insert(0) += count;
            *dp.entry(col + 1).or_insert(0) += count;
        }
        _ => {},
    });
    (splits, dp.values().sum())
}

fn main() {
    let start_time = Instant::now();
    let (splits, timelines) = both_parts(parse_input());
    let elapsed_time = start_time.elapsed();
    
    println!("Part 1: {}", splits);
    println!("Part 2: {}", timelines);
    println!("Elapsed: {:?}", elapsed_time);
}
