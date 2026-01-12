use itertools::Itertools;
use std::time::Instant;

struct Shape {
    height: usize,
    width: usize,
    cells: Vec<Vec<bool>>,
}

impl Shape {
    fn from_lines(lines: &[&str]) -> Self {
        let (height, width) = (lines.len() - 1, lines[1].len());
        let cells = lines[1..].iter().map(|line| line.chars().map(|c| c == '#').collect()).collect();
        Self { height, width, cells }
    }
}

struct Problem {
    height: usize,
    width: usize,
    shapes: Vec<usize>,
}

impl Problem {
    fn from_str(s: &str) -> Self {
        let (dimensions, shapes) = s.split_once(": ").unwrap();
        let (width, height) = dimensions.split("x").map(|s| s.parse().unwrap()).collect_tuple().unwrap();
        let shapes = shapes.split(" ").map(|s| s.parse().unwrap()).collect();
        Self { height, width, shapes }
    }
}

fn is_possible_naive(shapes: &[Shape], problem: &Problem) -> bool {
    if let Ok(sz) = shapes.iter().flat_map(|shape| [shape.width, shape.height]).all_equal_value() {
        let num_shapes = problem.shapes.iter().sum::<usize>();
        num_shapes <= (problem.width / sz) * (problem.height / sz)
    } else {
        false
    }
}

fn is_impossible_naive(shapes: &[Shape], problem: &Problem) -> bool {
    let cell_counts = shapes.iter().map(|shape| shape.cells.iter().flatten().filter(|&&x| x).count()).collect_vec();
    problem.shapes.iter().enumerate().map(|(idx, &cnt)| cnt * cell_counts[idx]).sum::<usize>() > problem.width * problem.height
}

fn parse_input() -> (Vec<Shape>, Vec<Problem>) {
    let groups = include_str!("../../../../inputs/2025/12.txt").lines()
        .chunk_by(|l| l.is_empty())
        .into_iter()
        .filter_map(|(empty, lines)| if !empty { Some(lines.collect_vec()) } else { None })
        .collect_vec();

    (
        groups[..groups.len() - 1].iter().map(|lines| Shape::from_lines(&lines)).collect(), 
        groups[groups.len() - 1].iter().map(|&s| Problem::from_str(s)).collect()
    )
}

fn part1(shapes: &[Shape], problems: &[Problem]) -> usize {
    problems.iter().filter(|problem| {
        if is_possible_naive(shapes, problem) { true }
        else { assert!(is_impossible_naive(shapes, problem)); false }
    }).count()
}

fn main() {
    let start_time = Instant::now();
    let (shapes, problems) = parse_input();
    let part1_ans = part1(&shapes, &problems);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", "Congratulations!");
    println!("Elapsed: {:?}", elapsed_time);
}
