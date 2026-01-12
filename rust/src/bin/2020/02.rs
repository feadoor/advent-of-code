use itertools::Itertools;
use std::time::Instant;

struct PasswordLine<'a> {
    lo: usize,
    hi: usize,
    c: char,
    password: &'a str,
}

impl<'a> PasswordLine<'a> {
    fn from_str(s: &'a str) -> Self {
        let (range, ch, password) = s.split_ascii_whitespace().collect_tuple().unwrap();
        let (lo, hi) = range.split("-").filter_map(|s| s.parse().ok()).collect_tuple().unwrap();
        let c = ch.chars().next().unwrap();
        Self { lo, hi, c, password }
    }
}

fn parse_input() -> Vec<PasswordLine<'static>> {
    include_str!("../../../../inputs/2020/02.txt").lines().map(PasswordLine::from_str).collect()
}

fn part1(passwords: &[PasswordLine]) -> usize {
    passwords.iter()
        .filter(|&&PasswordLine { lo, hi, c, password }| {
            let count = password.chars().filter(|&ch| ch == c).count();
            lo <= count && count <= hi
        })
        .count()
}

fn part2(passwords: &[PasswordLine]) -> usize {
    passwords.iter()
        .filter(|&&PasswordLine { lo, hi, c, password }| 
            password.chars().skip(lo - 1).step_by(hi - lo).take(2).filter(|&ch| ch == c).count() == 1
        )
        .count()
}

fn main() {
    let start_time = Instant::now();
    let passwords = parse_input();
    let part1_ans = part1(&passwords);
    let part2_ans = part2(&passwords);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
