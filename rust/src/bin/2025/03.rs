use std::time::Instant;

fn maximum_joltage(bank: &[usize], size: usize) -> usize {
    let first_digit = *bank[..bank.len() + 1 - size].iter().max().unwrap();
    if size == 1 { first_digit } else {
        let position = (0 .. bank.len()).position(|idx| bank[idx] == first_digit).unwrap();
        let rest = maximum_joltage(&bank[position + 1..], size - 1);
        10usize.pow(size as u32 - 1) * first_digit + rest
    }
}

fn parse_input() -> Vec<Vec<usize>> {
    include_str!("../../../../inputs/2025/03.txt").lines()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap() as usize).collect())
        .collect()
}

fn part1(banks: &Vec<Vec<usize>>) -> usize {
    banks.iter().map(|bank| maximum_joltage(bank, 2)).sum()
}

fn part2(banks: &Vec<Vec<usize>>) -> usize {
    banks.iter().map(|bank| maximum_joltage(bank, 12)).sum()
}

fn main() {
    let start_time = Instant::now();
    let banks = parse_input();
    let part1_ans = part1(&banks);
    let part2_ans = part2(&banks);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
