use advent_of_code::grid::{Direction, make_move};
use std::collections::{HashMap, HashSet, VecDeque};
use std::time::Instant;

fn convert_to_map(regex: &str) -> HashMap<(isize, isize), HashSet<Direction>> {
    let mut map = HashMap::new();
    let (mut position, mut position_stack) = ((0, 0), Vec::new());
    for c in regex.chars() {
        match c {
            'N' | 'E' | 'S' | 'W' => {
                let direction = Direction::from_compass_point(c);
                map.entry(position).or_insert(HashSet::new()).insert(direction);
                position = make_move(position, direction);
                map.entry(position).or_insert(HashSet::new()).insert(direction.reverse());
            },
            '(' => position_stack.push(position),
            '|' => position = *position_stack.last().unwrap(),
            ')' => { position_stack.pop(); },
            '^' | '$' => assert!(position_stack.is_empty()),
            _ => panic!("Unexpected character {}", c),
        }
    }
    map
}

fn distance_counts(map: &HashMap<(isize, isize), HashSet<Direction>>) -> HashMap<usize, usize> {
    let (mut bfs, mut visited, mut distances) = (VecDeque::from([(0, 0, 0)]), HashSet::from([(0, 0)]), HashMap::new());
    while let Some((y, x, d)) = bfs.pop_front() {
        for &direction in map.get(&(y, x)).into_iter().flatten() {
            let (new_y, new_x) = make_move((y, x), direction);
            if !visited.contains(&(new_y, new_x)) {
                visited.insert((new_y, new_x));
                bfs.push_back((new_y, new_x, d + 1));
                *distances.entry(d + 1).or_insert(0) += 1;
            }
        }
    }
    distances
}

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2018/20.txt").trim()
}

fn part1(distances: &HashMap<usize, usize>) -> usize {
    *distances.keys().max().unwrap()
}

fn part2(distances: &HashMap<usize, usize>) -> usize {
    distances.iter().filter_map(|(&k, &v)| (k >= 1000).then_some(v)).sum()
}

fn main() {
    let start_time = Instant::now();
    let regex = parse_input();
    let distances = distance_counts(&convert_to_map(regex));
    let part1_ans = part1(&distances);
    let part2_ans = part2(&distances);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
