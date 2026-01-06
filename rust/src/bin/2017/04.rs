use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2017/04.txt")
}

fn part1(passphrases: &str) -> usize {
    passphrases.lines().filter(|line| line.split(" ").all_unique()).count()
}

fn part2(passphrases: &str) -> usize {
    passphrases.lines().filter(|line| line.split(" ").map(|word| word.chars().sorted().collect::<String>()).all_unique()).count()
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(&input);
    let part2_ans = part2(&input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
