use itertools::{Itertools, iproduct};
use std::collections::HashMap; 
use std::time::Instant;

struct Claim {
    id: usize,
    left: usize,
    top: usize,
    width: usize,
    height: usize,
}

impl Claim {
    fn from_str(s: &str) -> Self {
        let (id, left, top, width, height) = s.split(|c: char| !c.is_numeric()).filter_map(|s| s.parse().ok()).collect_tuple().unwrap();
        Self { id, left, top, width, height }
    }

    fn cells(&self) -> impl Iterator<Item = (usize, usize)> {
        iproduct!((self.left..).take(self.width), (self.top..).take(self.height))
    }
}

fn create_map(claims: &[Claim]) -> HashMap<(usize, usize), usize> {
    let mut result = HashMap::new();
    claims.iter().flat_map(|claim| claim.cells()).for_each(|cell| {
        *result.entry(cell).or_insert(0) += 1;
    });
    result
}

fn parse_input() -> Vec<Claim> {
    include_str!("../../../../inputs/2018/03.txt").lines().map(Claim::from_str).collect()
}

fn part1(map: &HashMap<(usize, usize), usize>) -> usize {
    map.values().filter(|&&v| v > 1).count()
}

fn part2(claims: &[Claim], map: &HashMap<(usize, usize), usize>) -> usize {
    claims.iter().find(|&claim| claim.cells().all(|cell| map[&cell] == 1)).unwrap().id
}

fn main() {
    let start_time = Instant::now();
    let claims = parse_input();
    let map = create_map(&claims);
    let part1_ans = part1(&map);
    let part2_ans = part2(&claims, &map);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
