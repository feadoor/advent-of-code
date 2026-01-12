use std::collections::HashSet;
use std::time::Instant;

struct Group(Vec<HashSet<char>>);

impl Group {
    fn from_str(s: &str) -> Self {
        Self(s.lines().map(|line| line.chars().filter(|c| c.is_ascii_lowercase()).collect()).collect())
    }

    fn count_union(&self) -> usize {
        self.0.iter().fold(self.0[0].clone(), |acc, x | &acc | x).len()
    }

    fn count_intersection(&self) -> usize {
        self.0.iter().fold(self.0[0].clone(), |acc, x | &acc & x).len()
    }
}

fn parse_input() -> Vec<Group> {
    include_str!("../../../../inputs/2020/06.txt")
        .split("\n\n").flat_map(|s| s.split("\n\r\n"))
        .map(Group::from_str).collect()
}

fn part1(groups: &[Group]) -> usize {
    groups.iter().map(|g| g.count_union()).sum()
}

fn part2(groups: &[Group]) -> usize {
    groups.iter().map(|g| g.count_intersection()).sum()
}

fn main() {
    let start_time = Instant::now();
    let groups = parse_input();
    let part1_ans = part1(&groups);
    let part2_ans = part2(&groups);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
