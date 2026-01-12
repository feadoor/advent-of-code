use advent_of_code::grid::eight_neighbours;
use itertools::Itertools;
use std::iter::successors;
use std::time::Instant;

fn accessible_indices(grid: &Vec<Vec<bool>>) -> impl Iterator<Item = (usize, usize)> {
    (0 .. grid.len()).flat_map(|r| (0 .. grid[r].len())
        .map(move |c| (r, c))
        .filter(|&(r, c)|
            grid[r][c] && eight_neighbours((r, c), grid).into_iter().filter(|&(b, a)| grid[b][a]).count() < 4
        )
    )
}

fn parse_input() -> Vec<Vec<bool>> {
    let lines = include_str!("../../../../inputs/2025/04.txt").lines();
    lines.map(|line| line.chars().map(|c| c == '@').collect()).collect()
}

fn part1(grid: &Vec<Vec<bool>>) -> usize {
    accessible_indices(grid).count()
}

fn part2(grid: &mut Vec<Vec<bool>>) -> usize {
    successors(Some(0), |_| {
        let indices = accessible_indices(grid).collect_vec();
        for &(r, c) in &indices { grid[r][c] = false; }
        if indices.len() > 0 { Some(indices.len()) } else { None }
    }).sum()
}

fn main() {
    let start_time = Instant::now();
    let mut grid = parse_input();
    let part1_ans = part1(&grid);
    let part2_ans = part2(&mut grid);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
