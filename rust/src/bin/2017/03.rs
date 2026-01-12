use std::collections::HashMap;
use std::iter::{once, repeat_n};
use std::time::Instant;

fn sum_neighbours(values: &HashMap<(isize, isize), isize>, position: (isize, isize)) -> isize {
    [(-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1)].into_iter()
        .map(|(b, a)| values.get(&(position.0 + b, position.1 + a)).unwrap_or(&0))
        .sum()
}

fn parse_input() -> isize {
    include_str!("../../../../inputs/2017/03.txt").trim().parse().unwrap()
}

fn part1(index: isize) -> isize {
    if index == 1 { return 0; }
    let layer = (0..).take_while(|&l| (2 * l + 1) * (2 * l + 1) < index).last().unwrap() + 1;
    let value = (2 * layer - 1) * (2 * layer - 1) + 1;
    
    if value + 2 * layer - 1 >= index { (layer - 1 - (index - value)).abs() + layer }
    else if value + 4 * layer - 1 >= index { layer + (layer - (index - value - 2 * layer + 1)).abs() }
    else if value + 6 * layer - 1 >= index { (-layer + (index - value - 4 * layer + 1)).abs() + layer }
    else { layer + (-layer + (index - value - 6 * layer + 1)).abs() }
}

fn part2(index: isize) -> isize {
    let deltas = once((0, 1)).chain(
        (1..).flat_map(|layer| 
            repeat_n((-1, 0), 2 * layer - 1)
            .chain(repeat_n((0, -1), 2 * layer))
            .chain(repeat_n((1, 0), 2 * layer))
            .chain(repeat_n((0, 1), 2 * layer + 1))
        )
    );
    
    let mut values = HashMap::new();
    let mut position = (0, 0); values.insert(position, 1);

    for (dy, dx) in deltas {
        position = (position.0 + dy, position.1 + dx);
        let new_value = sum_neighbours(&values, position);
        if new_value > index { return new_value; }
        values.insert(position, new_value);
    }

    unreachable!()
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(input);
    let part2_ans = part2(input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
