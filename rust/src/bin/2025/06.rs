use itertools::Itertools;
use std::mem::take;
use std::time::Instant;

enum Operator {
    Plus,
    Times,
}

impl Operator {
    fn from_str(s: &str) -> Self {
        match s {
            "+" => Self::Plus,
            "*" => Self::Times,
            x => panic!("Unknown operator {}", x),
        }
    }

    fn from_char(c: char) -> Self {
        Self::from_str(&c.to_string())
    }
}

fn parse_input_part1() -> Vec<(Vec<usize>, Operator)> {
    let mut lines = include_str!("../../../../inputs/2025/06.txt").lines().map(|line| line.split_ascii_whitespace()).collect_vec();
    let operators = lines.pop().unwrap();
    operators.map(Operator::from_str).map(|op| {
        let numbers = lines.iter_mut().map(|line| line.next().unwrap().parse().unwrap()).collect();
        (numbers, op)
    }).collect()
}

fn parse_input_part2() -> Vec<(Vec<usize>, Operator)> {
    let mut lines = include_str!("../../../../inputs/2025/06.txt").lines().map(|line| line.chars().rev()).collect_vec();
    let operators = lines.pop().unwrap();
    let (mut problems, mut numbers) = (vec![], vec![]);
    operators.for_each(|op_char| {
        let digits = lines.iter_mut().map(|line| line.next().unwrap()).filter(|&c| c.is_numeric());
        if let Some(number) = digits.map(|d| d.to_digit(10).unwrap() as usize).reduce(|a, b| 10 * a + b) { numbers.push(number); }
        if !op_char.is_whitespace() { problems.push((take(&mut numbers), Operator::from_char(op_char))); }
    });
    problems
}

fn solve_problems(problems: &[(Vec<usize>, Operator)]) -> usize {
    problems.iter().map(|(vals, op)| match op {
        Operator::Plus => vals.iter().sum::<usize>(),
        Operator::Times => vals.iter().product::<usize>(),
    }).sum()
}

fn main() {
    let start_time = Instant::now();
    let part1 = solve_problems(&parse_input_part1());
    let part2 = solve_problems(&parse_input_part2());
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    println!("Elapsed: {:?}", elapsed_time);
}
