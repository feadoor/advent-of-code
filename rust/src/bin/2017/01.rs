use std::time::Instant;

fn sum_matching_digits(digits: &[usize], offset: usize) -> usize {
    digits.iter()
        .zip(digits.iter().cycle().skip(offset))
        .filter_map(|(&a, &b)| if a == b { Some(a) } else { None })
        .sum()
}

fn parse_input() -> Vec<usize> {
    let input = include_str!("../../../../inputs/2017/01.txt").trim();
    input.chars().map(|c| c.to_digit(10).unwrap() as usize).collect()
}

fn part1(digits: &[usize]) -> usize {
    sum_matching_digits(digits, 1)
}

fn part2(digits: &[usize]) -> usize {
    sum_matching_digits(digits, digits.len() / 2)
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
