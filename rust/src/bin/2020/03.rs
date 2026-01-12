use itertools::iterate;
use std::time::Instant;

fn trees_on_slope((dy, dx): (usize, usize), trees: &Vec<Vec<bool>>) -> usize {
    iterate((0, 0), |&(y, x)| (y + dy, x + dx))
        .take_while(|&(y, _)| y < trees.len())
        .filter(|&(y, x)| trees[y][x % trees[y].len()])
        .count()
}

fn parse_input() -> Vec<Vec<bool>> {
    include_str!("../../../../inputs/2020/03.txt").lines()
        .map(|line| line.chars().map(|c| c == '#').collect())
        .collect()
}

fn part1(trees: &Vec<Vec<bool>>) -> usize {
    trees_on_slope((1, 3), trees)
}

fn part2(trees: &Vec<Vec<bool>>) -> usize {
    [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)].into_iter().map(|slope| trees_on_slope(slope, trees)).product()
}

fn main() {
    let start_time = Instant::now();
    let trees = parse_input();
    let part1_ans = part1(&trees);
    let part2_ans = part2(&trees);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
