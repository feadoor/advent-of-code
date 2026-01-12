use itertools::Itertools;
use std::time::Instant;

const COLS: usize = 25;
const ROWS: usize = 6;

fn char_for(n: usize) -> Option<char> {
    match n {
        0 => Some(' '),
        1 => Some('#'),
        _ => None,
    }
}

fn parse_input() -> Vec<Vec<Vec<usize>>> {
    include_str!("../../../../inputs/2019/08.txt").trim().chars()
        .map(|c| c.to_digit(10).unwrap() as usize)
        .chunks(ROWS * COLS).into_iter()
        .map(|layer| layer.chunks(COLS).into_iter().map(|row| row.collect_vec()).collect_vec())
        .collect_vec()
}

fn part1(layers: &Vec<Vec<Vec<usize>>>) -> usize {
    let counts = layers.iter().map(|layer| layer.iter().flatten().counts())
        .min_by_key(|counts| counts[&0])
        .unwrap();
    counts[&1] * counts[&2]
}

fn part2(layers: &Vec<Vec<Vec<usize>>>) -> String {
    let compressed = (0 .. ROWS).map(|r| (0 .. COLS).map(move |c|
        layers.iter().filter_map(|layer| char_for(layer[r][c])).next().unwrap()
    ));
    compressed.map(|mut row| row.join("")).join("\n")
}

fn main() {
    let start_time = Instant::now();
    let layers = parse_input();
    let part1_ans = part1(&layers);
    let part2_ans = part2(&layers);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: \n\n{}\n", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
