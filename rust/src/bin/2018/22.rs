use advent_of_code::grid::four_neighbours_half_open;
use itertools::{Itertools, iproduct};
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::time::Instant;

fn get_erosion_level((y, x): (usize, usize), depth: usize, memo: &mut HashMap<(usize, usize), usize>) -> usize {
    if let Some(&v) = memo.get(&(y, x)) { v }
    else {
        let geologic_index =
        if y == 0 { 16_807 * x }
        else if x == 0 { 48_271 * y }
        else { get_erosion_level((y - 1, x), depth, memo) * get_erosion_level((y, x - 1), depth, memo) };
        let result = (geologic_index + depth) % 20_183;
        memo.insert((y, x), result); result
    }
}

fn neighbours((y, x, tool): (usize, usize, usize), depth: usize, erosion_levels: &mut HashMap<(usize, usize), usize>) -> Vec<(usize, (usize, usize, usize))> {
    let region_type = get_erosion_level((y, x), depth, erosion_levels) % 3;
    let mut result = Vec::new();
    for new_tool in (0 .. 3).filter(|&it| it != tool && it != region_type) { result.push((7, (y, x, new_tool))); }
    for (new_y, new_x) in four_neighbours_half_open((y, x)).into_iter() {
        if tool != get_erosion_level((new_y, new_x), depth, erosion_levels) % 3 {
            result.push((1, (new_y, new_x, tool)));
        }
    }
    result
}

fn shortest_path(start: (usize, usize, usize), target: (usize, usize, usize), depth: usize, erosion_levels: &mut HashMap<(usize, usize), usize>) -> usize {
    let mut locked = HashSet::new();
    let mut distances = HashMap::from([(start, 0)]);
    let mut pq = BinaryHeap::from([Reverse((0, start))]);
    while let Some(Reverse((distance, (y, x, tool)))) = pq.pop() {
        if (y, x, tool) == target { return distance; }
        else if !locked.contains(&(y, x, tool)) {
            for (d, (new_y, new_x, new_tool)) in neighbours((y, x, tool), depth, erosion_levels) {
                if !distances.get(&(new_y, new_x, new_tool)).is_some_and(|&dist| dist <= d + distance) {
                    distances.insert((new_y, new_x, new_tool), d + distance);
                    pq.push(Reverse((d + distance, (new_y, new_x, new_tool))));
                }
            }
            locked.insert((y, x, tool));
        }
    }
    unreachable!()
}

fn parse_input() -> (usize, (usize, usize)) {
    let (depth_line, target_line) = include_str!("../../../../inputs/2018/22.txt").lines().collect_tuple().unwrap();
    let depth = depth_line.split_once(": ").unwrap().1.parse().unwrap();
    let (x, y) = target_line.split_once(": ").unwrap().1.split(",").map(|s| s.parse().unwrap()).collect_tuple().unwrap();
    (depth, (y, x))
}

fn part1(target: (usize, usize), depth: usize, erosion_levels: &mut HashMap<(usize, usize), usize>) -> usize {
    iproduct!(0 ..= target.0, 0 ..= target.1).map(|(y, x)| get_erosion_level((y, x), depth, erosion_levels) % 3).sum()
}

fn part2((y, x): (usize, usize), depth: usize, erosion_levels: &mut HashMap<(usize, usize), usize>) -> usize {
    shortest_path((0, 0, 1), (y, x, 1), depth, erosion_levels)
}

fn main() {
    let start_time = Instant::now();
    let (depth, target) = parse_input();
    let mut erosion_levels = HashMap::new();
    let part1_ans = part1(target, depth, &mut erosion_levels);
    let part2_ans = part2(target, depth, &mut erosion_levels);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
