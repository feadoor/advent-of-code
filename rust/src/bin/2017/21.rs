use itertools::{Itertools, iproduct, iterate};
use std::collections::HashMap;
use std::time::Instant;

fn parse_grid(s: &str) -> Vec<Vec<bool>> {
    s.split("/").map(|row| row.chars().map(|c| c == '#').collect()).collect()
}

fn rotate(grid: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    (0 .. grid.len()).map(|r| (0 .. grid.len()).map(|c| grid[grid.len() - c - 1][r]).collect()).collect()
}

fn reflect(grid: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    (0 .. grid.len()).map(|r| (0 .. grid.len()).map(|c| grid[c][r]).collect()).collect()
}

fn all_transformations(grid: &Vec<Vec<bool>>) -> impl Iterator<Item = Vec<Vec<bool>>> {
    iterate(grid.clone(), |g| rotate(g))
        .take(4)
        .flat_map(|g| [reflect(&g), g])
}

fn all_transition_rules(rules: &[(Vec<Vec<bool>>, Vec<Vec<bool>>)]) -> HashMap<Vec<Vec<bool>>, Vec<Vec<bool>>> {
    rules.iter()
        .flat_map(|(input, output)| all_transformations(input).map(|grid| (grid, output.clone())))
        .collect()
}

fn get_subgrid(grid: &Vec<Vec<bool>>, row: usize, col: usize, size: usize) -> Vec<Vec<bool>> {
    (row .. row + size).map(|r| (col .. col + size).map(|c| grid[r][c]).collect()).collect()
}

fn set_subgrid(grid: &mut Vec<Vec<bool>>, row: usize, col: usize, pattern: &Vec<Vec<bool>>) {
    iproduct!(0 .. pattern.len(), 0 .. pattern.len()).for_each(|(r, c)| grid[row + r][col + c] = pattern[r][c])
}

fn transform(grid: &Vec<Vec<bool>>, rules: &HashMap<Vec<Vec<bool>>, Vec<Vec<bool>>>) -> Vec<Vec<bool>> {
    let input_size = grid.len();
    let (input_pattern_size, output_pattern_size) = if input_size % 2 == 0 { (2, 3) } else { (3, 4) };
    let output_size = (input_size / input_pattern_size) * output_pattern_size;
    let mut output = vec![vec![false; output_size]; output_size];

    for row in 0 .. input_size / input_pattern_size {
        for col in 0 .. input_size / input_pattern_size {
            let subgrid = get_subgrid(grid, row * input_pattern_size, col * input_pattern_size, input_pattern_size);
            let enhanced = rules[&subgrid].clone();
            set_subgrid(&mut output, row * output_pattern_size, col * output_pattern_size, &enhanced);
        }
    }

    output
}

fn transform_n_times(n: usize, grid: &Vec<Vec<bool>>, rules: &HashMap<Vec<Vec<bool>>, Vec<Vec<bool>>>) -> Vec<Vec<bool>> {
    iterate(grid.clone(), |grid| transform(grid, rules)).nth(n).unwrap()
}

fn count_set_cells(grid: &Vec<Vec<bool>>) -> usize {
    grid.iter().map(|row| row.iter().filter(|&&x| x).count()).sum()
}

fn parse_input() -> Vec<(Vec<Vec<bool>>, Vec<Vec<bool>>)> {
    include_str!("../../../../inputs/2017/21.txt").lines()
        .map(|line| line.split(" => ").map(parse_grid).collect_tuple().unwrap())
        .collect()
}

fn main() {
    let start_time = Instant::now();
    let rules = all_transition_rules(&parse_input());
    let start = vec![vec![false, true, false], vec![false, false, true], vec![true, true, true]];
    let part1 = transform_n_times(5, &start, &rules);
    let part2 = transform_n_times(13, &part1, &rules);
    let part1_ans = count_set_cells(&part1);
    let part2_ans = count_set_cells(&part2);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
