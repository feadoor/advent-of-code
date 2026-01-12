use advent_of_code::grid::{Direction, make_move};
use advent_of_code::intcode::IntcodeRunner;
use std::collections::{HashMap, HashSet, VecDeque};
use std::collections::hash_map::Entry;
use std::time::Instant;

enum TraversalMode {
    Out,
    In,
}

fn input_for(direction: Direction) -> isize {
    match direction {
        Direction::Up => 1,
        Direction::Down => 2,
        Direction::Left => 3,
        Direction::Right => 4,
    }
}

fn fully_explore(program: Vec<isize>) -> (HashSet<(isize, isize)>, (isize, isize)) {
    use TraversalMode::*; use Direction::*;
    let mut robot = IntcodeRunner::for_program(program);
    let (mut checked, mut valid) = (HashSet::from([(0, 0)]), HashSet::from([(0, 0)]));
    let (mut pos, mut destination) = ((0, 0), None);
    let mut stack = vec![(Out, Up), (Out, Down), (Out, Left), (Out, Right)];

    while let Some((mode, dir)) = stack.pop() {
        robot.push_input(input_for(dir));
        let result = { robot.run(); robot.pop_output().unwrap() };
        if result != 0 {
            pos = make_move(pos, dir);
            if matches!(mode, Out) {
                valid.insert(pos); stack.push((In, dir.reverse()));
                if result == 2 { destination = Some(pos); }
                for next_dir in [Up, Down, Left, Right].into_iter() {
                    let next_pos = make_move(pos, next_dir);
                    if checked.insert(next_pos) { stack.push((Out, next_dir)); }
                }
            }
        }
    }

    (valid, destination.unwrap())
}

fn bfs(map: &HashSet<(isize, isize)>, src: (isize, isize)) -> HashMap<(isize, isize), usize> {
    let mut distances = HashMap::from([(src, 0)]);
    let mut queue = VecDeque::from([(src, 0)]);
    while let Some((pos, dist)) = queue.pop_front() {
        for dir in [Direction::Up, Direction::Down, Direction::Left, Direction::Right] {
            let next_pos = make_move(pos, dir);
            if map.contains(&next_pos) {
                if let Entry::Vacant(e) = distances.entry(next_pos) {
                    e.insert(dist + 1);
                    queue.push_back((next_pos, dist + 1));
                }
            }
        }
    }
    distances
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/15.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn both_parts(program: Vec<isize>) -> (usize, usize) {
    let (map, src) = fully_explore(program);
    let distances = bfs(&map, src);
    (distances[&(0, 0)], *distances.values().max().unwrap())
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let (part1_ans, part2_ans) = both_parts(program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
