use std::time::Instant;

const DIAL_SIZE: usize = 100;

enum Move {
    Left(usize),
    Right(usize),
}

impl Move {
    fn from_str(s: &str) -> Self {
        match &s[0..1] {
            "L" => Self::Left(s[1..].parse().unwrap()),
            "R" => Self::Right(s[1..].parse().unwrap()),
            x => panic!("Unknown direction {}", x),
        }
    }
}

fn invert(x: usize) -> usize {
    if x == 0 { 0 } else { DIAL_SIZE - x }
}

fn make_move_and_count_zeroes(x: usize, m: &Move) -> (usize, usize) {
    match m {
        Move::Right(val) => {
            let result = x + val;
            (result % DIAL_SIZE, result / DIAL_SIZE)
        }
        Move::Left(val) => {
            let result = invert(x) + val;
            (invert(result % DIAL_SIZE), result / DIAL_SIZE)
        }
    }
}

fn positions_sequence(start: usize, moves: &[Move]) -> impl Iterator<Item = (usize, usize)> {
    moves.iter()
        .scan((start, 0), |(pos, count), m| {
            let (new_pos, zeroes) = make_move_and_count_zeroes(*pos, m);
            *pos = new_pos; *count += zeroes;
            Some((*pos, *count))
        })
}

fn parse_input() -> Vec<Move> {
    include_str!("../../../../inputs/2025/01.txt").lines()
        .map(Move::from_str)
        .collect()
}

fn part1(moves: &[Move]) -> usize {
    positions_sequence(50, moves)
        .filter(|&(pos, _count)| pos == 0)
        .count()
}

fn part2(moves: &[Move]) -> usize {
    positions_sequence(50, moves).last().unwrap().1
}

fn main() {
    let start_time = Instant::now();
    let moves = parse_input();
    let part1_ans = part1(&moves);
    let part2_ans = part2(&moves);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
