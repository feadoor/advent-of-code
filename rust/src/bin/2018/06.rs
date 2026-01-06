use itertools::{Itertools, iproduct};
use std::time::Instant;

fn distance((y, x): (isize, isize), (b, a): (isize, isize)) -> usize {
    y.abs_diff(b) + x.abs_diff(a)
}

fn boundaries(locations: &[(isize, isize)]) -> (isize, isize, isize, isize) {
    let (top, bottom) = locations.iter().map(|l| l.0).minmax().into_option().unwrap();
    let (left, right) = locations.iter().map(|l| l.1).minmax().into_option().unwrap();
    (top, bottom, left, right)
}

fn parse_input() -> Vec<(isize, isize)> {
    include_str!("../../../../inputs/2018/06.txt").lines()
        .map(|line| line.split(", ").map(|s| s.parse().unwrap()).collect_tuple().unwrap())
        .collect()
}

fn part1(locations: &[(isize, isize)]) -> usize {
    let (top, bottom, left, right) = boundaries(locations);
    let mut result = vec![(0, false); locations.len()];

    iproduct!(top..=bottom, left..=right).for_each(|(y, x)| {
        let nearest_ids = (0 .. locations.len()).min_set_by_key(|&idx| distance(locations[idx], (y, x)));
        if let Ok(id) = nearest_ids.into_iter().exactly_one() {
            result[id].0 += 1;
            if y == bottom || y == top || x == left || x == right { result[id].1 = true; }
        }
    });
    result.into_iter().filter(|&(_, edge)| !edge).map(|(v, _)| v).max().unwrap()
}

fn part2(locations: &[(isize, isize)]) -> usize {
    let (top, bottom, left, right) = boundaries(locations);
    let buffer_size = (10_000 / locations.len() / 2) as isize;

    iproduct!((top - buffer_size) ..= (bottom + buffer_size), (left - buffer_size) ..= (right + buffer_size)).filter(|&p| {
        locations.iter().map(|&l| distance(l, p)).sum::<usize>() < 10_000
    }).count()
}

fn main() {
    let start_time = Instant::now();
    let locations = parse_input();
    let part1_ans = part1(&locations);
    let part2_ans = part2(&locations);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
