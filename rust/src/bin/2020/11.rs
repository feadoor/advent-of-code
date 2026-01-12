use advent_of_code::grid::eight_neighbours;
use advent_of_code::more_itertools::FirstDuplicateExt;
use itertools::iterate;
use std::iter::successors;
use std::time::Instant;

fn ray<T>(grid: &Vec<Vec<T>>, start: (usize, usize), next: (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
    let cells = successors(Some((start, next)), |&(prev, curr)| {
        (2 * curr.0).checked_sub(prev.0).and_then(|y| (2 * curr.1).checked_sub(prev.1).map(|x| (y, x))).map(|it| (curr, it))
    });
    cells.map(|(_prev, curr)| curr).take_while(|&(y, x)| y < grid.len() && x < grid[y].len())
}

fn occupied_neighbours(grid: &Vec<Vec<Option<bool>>>, (y, x): (usize, usize)) -> usize {
    eight_neighbours((y, x), grid).into_iter().filter(|&(b, a)| grid[b][a] == Some(true)).count()
}

fn occupied_long_neighbours(grid: &Vec<Vec<Option<bool>>>, (y, x): (usize, usize)) -> usize {
    let neighbours = eight_neighbours((y, x), grid);
    let lines = neighbours.into_iter().map(|nbr| ray(grid, (y, x), nbr));
    let seats = lines.map(|line| line.map(|(y, x)| grid[y][x]).find(|seat| seat.is_some()).flatten());
    seats.filter(|&seat| seat == Some(true)).count()
}

fn tick<F: Fn(&Vec<Vec<Option<bool>>>, (usize, usize)) -> usize>(grid: &Vec<Vec<Option<bool>>>, nbr_count: F, threshold: usize) -> Vec<Vec<Option<bool>>> {
    (0 .. grid.len()).map(|y| (0 .. grid[y].len()).map(|x| {
        grid[y][x].map(|seat| match seat {
            false => nbr_count(grid, (y, x)) == 0,
            true  => nbr_count(grid, (y, x)) < threshold,
        })
    }).collect()).collect()
}

fn parse_input() -> Vec<Vec<Option<bool>>> {
    include_str!("../../../../inputs/2020/11.txt").lines()
        .map(|line| line.chars().map(|c| (c != '.').then_some(c != 'L')).collect())
        .collect()
}

fn part1(seats: &Vec<Vec<Option<bool>>>) -> usize {
    iterate(seats.to_vec(), |seats| tick(seats, occupied_neighbours, 4))
        .first_duplicate().unwrap()
        .into_iter().flatten().filter(|&seat| seat == Some(true)).count()
}

fn part2(seats: &Vec<Vec<Option<bool>>>) -> usize {
    iterate(seats.to_vec(), |seats| tick(seats, occupied_long_neighbours, 5))
        .first_duplicate().unwrap()
        .into_iter().flatten().filter(|&seat| seat == Some(true)).count()
}

fn main() {
    let start_time = Instant::now();
    let seats = parse_input();
    let part1_ans = part1(&seats);
    let part2_ans = part2(&seats);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
