use itertools::Itertools;
use std::time::Instant;

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2018/02.txt")
}

fn part1(boxes: &str) -> usize {
    let (doubles, triples) = boxes.lines().fold((0, 0), |(doubles, triples), box_name| {
        let counts = box_name.chars().counts();
        let doubles = if counts.values().contains(&2) { doubles + 1 } else { doubles };
        let triples = if counts.values().contains(&3) { triples + 1 } else { triples };
        (doubles, triples)
    });
    doubles * triples
}

fn part2(boxes: &str) -> String {
    boxes.lines().tuple_combinations()
        .filter(|&(b1, b2)| b1.chars().zip(b2.chars()).filter(|&(c1, c2)| c1 != c2).nth(1).is_none())
        .map(|(b1, b2)| b1.chars().zip(b2.chars()).filter(|&(c1, c2)| c1 == c2).map(|(c1, _c2)| c1).collect())
        .next().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let boxes = parse_input();
    let part1_ans = part1(&boxes);
    let part2_ans = part2(&boxes);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
