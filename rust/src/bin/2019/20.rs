use advent_of_code::grid::four_neighbours;
use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};
use std::mem::swap;
use std::time::Instant;

enum Label {
    Outer { partner: Option<(usize, usize)> },
    Inner { partner: Option<(usize, usize)> },
}

impl Label {
    fn partner(&self) -> Option<(usize, usize)> {
        match self {
            Self::Outer { partner } => *partner,
            Self::Inner { partner } => *partner,
        }
    }
}

fn is_outer(maze: &Vec<Vec<char>>, (y, x): (usize, usize)) -> bool {
    y == 2 || y == maze.len() - 3 || x == 2 || x == maze[y].len() - 3
}

fn find_labels(maze: &Vec<Vec<char>>) -> ((usize, usize), (usize, usize), HashMap<(usize, usize), Label>) {
    let raw_labels = (0 .. maze.len()).flat_map(|y| (0 .. maze[y].len()).map(move |x| (y, x)))
        .filter(|&(y, x)| maze[y][x] == '.')
        .filter_map(|(y, x)|
            if y >= 2 && maze[y - 1][x].is_ascii_uppercase() && maze[y - 2][x].is_ascii_uppercase() {
                Some(((maze[y - 2][x], maze[y - 1][x]), (y, x)))
            }
            else if y < maze.len() - 2 && maze[y + 1][x].is_ascii_uppercase() && maze[y + 2][x].is_ascii_uppercase() {
                Some(((maze[y + 1][x], maze[y + 2][x]), (y, x)))
            }
            else if x >= 2 && maze[y][x - 1].is_ascii_uppercase() && maze[y][x - 2].is_ascii_uppercase() {
                Some(((maze[y][x - 2], maze[y][x - 1]), (y, x)))
            }
            else if x < maze[y].len() - 2 && maze[y][x + 1].is_ascii_uppercase() && maze[y][x + 2].is_ascii_uppercase() {
                Some(((maze[y][x + 1], maze[y][x + 2]), (y, x)))
            }
            else { None }
        )
        .into_group_map();

    let &start_pos = raw_labels.get(&('A', 'A')).unwrap().iter().exactly_one().unwrap();
    let &end_pos = raw_labels.get(&('Z', 'Z')).unwrap().iter().exactly_one().unwrap();

    let labels = raw_labels.into_values().flat_map(|mut positions| match positions.as_mut_slice() {
        &mut [mut pos1, mut pos2] => {
            if is_outer(maze, pos2) { swap(&mut pos1, &mut pos2); }
            [(pos1, Label::Outer { partner: Some(pos2) }), (pos2, Label::Inner { partner: Some(pos1) })].into_iter().collect_vec()
        }
        &mut [pos] => {
            if !is_outer(maze, pos) { panic!("Lonely labels must be on the outer level: {:?}", pos); }
            [(pos, Label::Outer { partner: None })].into_iter().collect_vec()
        }
        _ => panic!("Labels must occur alone or in pairs")
    }).collect();

    (start_pos, end_pos, labels)
}

fn parse_input() -> Vec<Vec<char>> {
    include_str!("../../../../inputs/2019/20.txt").lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn part1(maze: &Vec<Vec<char>>, labels: &HashMap<(usize, usize), Label>, start: (usize, usize), end: (usize, usize)) -> usize {
    let (mut queue, mut visited) = (VecDeque::from([(start, 0)]), HashSet::from([start]));
    while let Some((pos, dist)) = queue.pop_front() {
        if pos == end { return dist; }
        for (y, x) in four_neighbours(pos, maze) {
            if maze[y][x] == '.' && visited.insert((y, x)) {
                queue.push_back(((y, x), dist + 1));
            }
        }
        if let Some(partner) = labels.get(&pos).and_then(|label| label.partner()) {
            if visited.insert(partner) {
                queue.push_back((partner, dist + 1));
            }
        }
    }
    panic!("No path found");
}

fn part2(maze: &Vec<Vec<char>>, labels: &HashMap<(usize, usize), Label>, start: (usize, usize), end: (usize, usize)) -> usize {
    let (mut queue, mut visited) = (VecDeque::from([((start.0, start.1, 0), 0)]), HashSet::from([(start.0, start.1, 0)]));
    while let Some(((y, x, z), dist)) = queue.pop_front() {
        if (y, x, z) == (end.0, end.1, 0) { return dist; }
        for (y, x) in four_neighbours((y, x), maze) {
            if maze[y][x] == '.' && visited.insert((y, x, z)) {
                queue.push_back(((y, x, z), dist + 1));
            }
        }
        if let Some(label) = labels.get(&(y, x)) {
            match label {
                &Label::Outer { partner: Some(partner) } if z > 0 => {
                    if visited.insert((partner.0, partner.1, z - 1)) {
                        queue.push_back(((partner.0, partner.1, z - 1), dist + 1));
                    }
                }
                &Label::Inner { partner: Some(partner) } => {
                    if visited.insert((partner.0, partner.1, z + 1)) {
                        queue.push_back(((partner.0, partner.1, z + 1), dist + 1));
                    }
                }
                _ => {},
            }
        }
    }
    panic!("No path found");
}

fn main() {
    let start_time = Instant::now();
    let maze = parse_input();
    let (start, end, labels) = find_labels(&maze);
    let part1_ans = part1(&maze, &labels, start, end);
    let part2_ans = part2(&maze, &labels, start, end);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
