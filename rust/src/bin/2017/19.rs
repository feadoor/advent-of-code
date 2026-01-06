use advent_of_code::grid::{Direction, make_move};
use std::time::Instant;

fn is_in_path((y, x): (usize, usize), diagram: &Vec<Vec<char>>) -> bool {
    y < diagram.len() && x < diagram[y].len() && diagram[y][x] != ' '
}

fn parse_input() -> Vec<Vec<char>> {
    include_str!("../../../../inputs/2017/19.txt").lines()
        .map(|line| line.chars().collect())
        .collect()
}

fn follow_diagram(diagram: &Vec<Vec<char>>) -> (String, usize) {
    let (mut y, mut x) = (1, diagram[0].iter().position(|&c| c == '|').unwrap());
    let mut direction = Direction::Down;
    let (mut letters, mut steps) = (String::new(), 1);

    while is_in_path((y, x), diagram) {
        steps += 1;
        if diagram[y][x] == '+' {
            direction = [Direction::Up, Direction::Down, Direction::Left, Direction::Right].into_iter()
                .find(|&d| d != direction.reverse() && is_in_path(make_move((y, x), d), diagram))
                .unwrap_or(direction);
        } else if diagram[y][x].is_alphabetic() {
            letters.push(diagram[y][x]);
        }
        (y, x) = make_move((y, x), direction);
    }

    (letters, steps)
}

fn main() {
    let start_time = Instant::now();
    let diagram = parse_input();
    let (letters, steps) = follow_diagram(&diagram);
    let elapsed_time = start_time.elapsed();
    
    println!("Part 1: {}", letters);
    println!("Part 2: {}", steps);
    println!("Elapsed: {:?}", elapsed_time);
}
