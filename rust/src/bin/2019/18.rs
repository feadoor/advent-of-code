use advent_of_code::grid::four_neighbours;
use itertools::Itertools;
use std::collections::{BTreeSet, HashMap, VecDeque};
use std::time::Instant;

fn update_at<T: Clone>(idx: usize, vals: &[T], new_val: T) -> Vec<T> {
    let mut result = vals.to_vec();
    result[idx] = new_val;
    result
}

fn without<T: Copy + Clone + Ord>(set: &BTreeSet<T>, item: T) -> BTreeSet<T> {
    set.difference(&BTreeSet::from([item])).copied().collect()
}

fn matches(src: &Vec<Vec<char>>, (y, x): (usize, usize), target: &Vec<Vec<char>>) -> bool {
    (y..).zip(target.iter())
        .flat_map(|(b, row)| (x..).map(move |a| (b, a)).zip(row.iter()))
        .all(|((b, a), &c)| src[b][a] == c)
}

fn replace(maze: &Vec<Vec<char>>, target: &Vec<Vec<char>>, replacement: &Vec<Vec<char>>) -> Vec<Vec<char>> {
    let (y, x) = (0 .. maze.len() - 2).flat_map(|y| (0 .. maze[y].len() - 2).map(move |x| (y, x)))
        .find(|&(y, x)| matches(maze, (y, x), target))
        .unwrap();
    let mut result = maze.to_vec();
    (y..).zip(replacement.iter())
        .flat_map(|(b, row)| (x..).map(move |a| (b, a)).zip(row.iter()))
        .for_each(|((b, a), &c)| result[b][a] = c);
    result
}

fn shortest_paths_from(maze: &Vec<Vec<char>>, start_position: (usize, usize)) -> HashMap<char, ((usize, usize), usize, BTreeSet<char>)> {
    let mut queue = VecDeque::from([(start_position, 0, BTreeSet::new())]);
    let mut visited = BTreeSet::from([start_position]);
    let mut result = HashMap::new();

    while let Some((curr_pos, dist, req_keys)) = queue.pop_front() {
        for (y, x) in four_neighbours(curr_pos, maze) {
            if visited.insert((y, x)) {
                if maze[y][x] == '#' { continue; }
                if maze[y][x].is_ascii_lowercase() { result.insert(maze[y][x], ((y, x), dist + 1, req_keys.clone())); }
                if maze[y][x].is_ascii_alphabetic() { queue.push_back(((y, x), dist + 1, &req_keys | &BTreeSet::from([maze[y][x].to_ascii_lowercase()]))); } 
                else { queue.push_back(((y, x), dist + 1, req_keys.clone())); }
            }
        }
    }

    result
}

fn shortest_path_inner(paths: &HashMap<(usize, usize), HashMap<char, ((usize, usize), usize, BTreeSet<char>)>>, positions: Vec<(usize, usize)>, keys_remaining: BTreeSet<char>, memo: &mut HashMap<Vec<(usize, usize)>, HashMap<BTreeSet<char>, usize>>) -> usize {
    if keys_remaining.len() == 0 { return 0; }
    if let Some(&result) = memo.get(&positions).and_then(|sub_memo| sub_memo.get(&keys_remaining)) { return result; }
    
    let result = (0 .. positions.len()).flat_map(|idx| paths[&positions[idx]].iter().map(move |item| (idx, item)))
        .filter(|(_, (key, (_, _, req_keys)))| keys_remaining.contains(key) && req_keys.iter().all(|k| !keys_remaining.contains(k)))
        .map(|(idx, (key, (pos, dist, _)))| 
            dist + shortest_path_inner(paths, update_at(idx, &positions, *pos), without(&keys_remaining, *key), memo)
        ).min().unwrap();
    
    memo.entry(positions).or_default().insert(keys_remaining, result);
    result
}

fn shortest_path_for_all_keys(maze: &Vec<Vec<char>>) -> usize {
    let start_positions = (0 .. maze.len()).flat_map(|y| (0 .. maze[y].len()).filter_map(move |x| (maze[y][x] == '@').then_some((y, x)))).collect_vec();
    let keys: HashMap<_, _> = (0 .. maze.len()).flat_map(|y| (0 .. maze[y].len()).filter_map(move |x| maze[y][x].is_ascii_lowercase().then_some(((y, x), maze[y][x])))).collect();
    let paths = start_positions.iter().copied().chain(keys.keys().copied()).map(|pos| (pos, shortest_paths_from(maze, pos))).collect();
    shortest_path_inner(&paths, start_positions, keys.values().into_iter().copied().collect(), &mut HashMap::new())
}

fn parse_input() -> Vec<Vec<char>> {
    include_str!("../../../../inputs/2019/18.txt").lines()
        .map(|s| s.chars().collect())
        .collect()
}

fn part1(maze: &Vec<Vec<char>>) -> usize {
    shortest_path_for_all_keys(maze)
}

fn part2(maze: &Vec<Vec<char>>) -> usize {
    let target = vec![vec!['.', '.', '.'], vec!['.', '@', '.'], vec!['.', '.', '.']];
    let replacement = vec![vec!['@', '#', '@'], vec!['#', '#', '#'], vec!['@', '#', '@']];
    shortest_path_for_all_keys(&replace(maze, &target, &replacement))
}

fn main() {
    let start_time = Instant::now();
    let maze = parse_input();
    let part1_ans = part1(&maze);
    let part2_ans = part2(&maze);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
