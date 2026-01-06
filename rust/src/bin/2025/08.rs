use advent_of_code::union_find::UnionFind;
use itertools::Itertools;
use std::time::Instant;

type Tuple3 = (isize, isize, isize);

fn squared_distance((x1, y1, z1): Tuple3, (x2, y2, z2): Tuple3) -> isize {
    (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)
}

fn sorted_pairs(positions: &[Tuple3]) -> impl Iterator<Item = (usize, usize)> {
    (0 .. positions.len())
        .tuple_combinations()
        .sorted_by_key(|&(idx, jdx)| squared_distance(positions[idx], positions[jdx]))
}

fn parse_input() -> Vec<Tuple3> {
    include_str!("../../../../inputs/2025/08.txt").lines()
        .map(|line| line.split(",").map(|s| s.parse().unwrap()).collect_tuple().unwrap())
        .collect()
}

fn part1<I: Iterator<Item = (usize, usize)>>(circuits: &mut UnionFind, connections: &mut I) -> usize {
    connections.take(1_000).for_each(|(idx, jdx)| circuits.merge(idx, jdx));
    circuits.all_set_sizes().k_largest(3).product()
}

fn part2<I: Iterator<Item = (usize, usize)>>(positions: &[Tuple3], circuits: &mut UnionFind, connections: &mut I) -> isize {
    loop {
        let (idx, jdx) = connections.next().unwrap();
        circuits.merge(idx, jdx);
        if circuits.size_of_set_containing(0) == positions.len() {
            return positions[idx].0 * positions[jdx].0;
        }
    }
}

fn main() {
    let start_time = Instant::now();
    let positions = parse_input();
    let mut circuits = UnionFind::new(positions.len());
    let mut connections = sorted_pairs(&positions);
    let part1_ans = part1(&mut circuits, &mut connections);
    let part2_ans = part2(&positions, &mut circuits, &mut connections);
    let elapsed_time = start_time.elapsed();
    
    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
