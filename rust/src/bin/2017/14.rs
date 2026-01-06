use advent_of_code::grid::four_neighbours;
use advent_of_code::knot_hash::compute_hash;
use itertools::{Itertools, iterate};
use std::iter::repeat;
use std::time::Instant;

fn bits(val: u8) -> impl Iterator<Item = bool> {
    iterate(val, |x| x / 2).take_while(|&x| x != 0).map(|x| x % 2 == 1).chain(repeat(false)).take(8)
}

fn compute_grid(input: &str) -> Vec<Vec<bool>> {
    (0 .. 128).map(|idx| {
        let hash_input = format!("{}-{}", input, idx).bytes().map(|b| b as usize).collect_vec();
        let dense_hash = compute_hash(&hash_input);
        dense_hash.into_iter().rev().flat_map(bits).collect()
    }).collect()
}

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2017/14.txt").trim()
}

fn part1(grid: &Vec<Vec<bool>>) -> usize {
    grid.iter().map(|row| row.iter().filter(|&&x| x).count()).sum()
}

fn part2(grid: &Vec<Vec<bool>>) -> usize {
    let mut visited = grid.iter().map(|row| row.iter().map(|_| false).collect_vec()).collect_vec();
    let mut component_count = 0;

    for row in 0 .. grid.len() {
        for col in 0 .. grid[row].len() {
            if grid[row][col] && !visited[row][col] {
                component_count += 1;
                visited[row][col] = true;
                let mut queue = vec![(row, col)];
                while let Some((r, c)) = queue.pop() {
                    for (b, a) in four_neighbours((r, c), grid) {
                        if grid[b][a] && !visited[b][a] {
                            visited[b][a] = true;
                            queue.push((b, a));
                        }
                    }
                }
            }
        }
    }

    component_count
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let grid = compute_grid(&input);
    let part1_ans = part1(&grid);
    let part2_ans = part2(&grid);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
