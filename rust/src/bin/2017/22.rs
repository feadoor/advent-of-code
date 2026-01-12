use advent_of_code::grid::{Direction, make_move};
use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

#[derive(Copy, Clone, PartialEq, Eq)]
enum CellState {
    Clean,
    Weakened,
    Infected,
    Flagged,
}

fn step_part1(position: &mut (isize, isize), direction: &mut Direction, cells: &mut HashMap<(isize, isize), CellState>, ) -> CellState {
    let current_state = *cells.entry(*position).or_insert(CellState::Clean);
    let new_state = match current_state {
        CellState::Clean => { *direction = direction.turn_left(); CellState::Infected },
        CellState::Infected => { *direction = direction.turn_right(); CellState::Clean },
        _ => unreachable!(),
    };

    cells.insert(*position, new_state);
    *position = make_move(*position, *direction);

    new_state
}

fn step_part2(position: &mut (isize, isize), direction: &mut Direction, cells: &mut HashMap<(isize, isize), CellState>, ) -> CellState {
    let current_state = *cells.entry(*position).or_insert(CellState::Clean);
    let new_state = match current_state {
        CellState::Clean => { *direction = direction.turn_left(); CellState::Weakened },
        CellState::Infected => { *direction = direction.turn_right(); CellState::Flagged },
        CellState::Flagged => { *direction = direction.reverse(); CellState::Clean },
        CellState::Weakened => CellState::Infected,
    };

    cells.insert(*position, new_state);
    *position = make_move(*position, *direction);

    new_state
}

fn cell_states<F>(position: (isize, isize), direction: Direction, cells: HashMap<(isize, isize), CellState>, mut step: F) -> impl Iterator<Item = CellState> 
where F: FnMut(&mut (isize, isize), &mut Direction, &mut HashMap<(isize, isize), CellState>) -> CellState + 'static
{
    (0..).scan((position, direction, cells), move |(pos, dir, map), _| Some(step(pos, dir, map)))
}

fn parse_input() -> ((isize, isize), HashMap<(isize, isize), CellState>) {
    let grid = include_str!("../../../../inputs/2017/22.txt").lines().collect_vec();
    let start_position = (grid.len() as isize / 2, grid.len() as isize / 2);
    let cells = grid.iter().enumerate().flat_map(|(row, line)| 
        line.chars().enumerate().map(move |(col, c)| {
            let cell_state = if c == '#' { CellState::Infected } else { CellState::Clean };
            ((row as isize, col as isize), cell_state)
        })
    ).collect();

    (start_position, cells)
}

fn part1(position: (isize, isize), direction: Direction, cells: HashMap<(isize, isize), CellState>) -> usize {
    cell_states(position, direction, cells, step_part1)
        .take(10_000)
        .filter(|&state| state == CellState::Infected)
        .count()
}

fn part2(position: (isize, isize), direction: Direction, cells: HashMap<(isize, isize), CellState>) -> usize {
    cell_states(position, direction, cells, step_part2)
        .take(10_000_000)
        .filter(|&state| state == CellState::Infected)
        .count()
}

fn main() {
    let start_time = Instant::now();
    let (start_position, cells) = parse_input();
    let start_direction = Direction::Up;
    let part1_ans = part1(start_position, start_direction, cells.clone());
    let part2_ans = part2(start_position, start_direction, cells);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
