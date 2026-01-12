use itertools::{Itertools, iterate};
use std::time::Instant;

type Tuple2 = (isize, isize);

struct Point {
    position: Tuple2,
    velocity: Tuple2,
}

impl Point {
    fn moved(&self, ticks: isize) -> Tuple2 {
        (self.position.0 + ticks * self.velocity.0, self.position.1 + ticks * self.velocity.1)
    }
}

fn bounding_box(points: &[Point], ticks: isize) -> (Tuple2, Tuple2) {
    let (min_x, max_x) = points.iter().map(|point| point.moved(ticks).0).minmax().into_option().unwrap();
    let (min_y, max_y) = points.iter().map(|point| point.moved(ticks).1).minmax().into_option().unwrap();
    ((min_x, max_x), (min_y, max_y))
}

fn size_after_ticks(points: &[Point], ticks: isize) -> isize {
    let ((min_x, max_x), (min_y, max_y)) = bounding_box(points, ticks);
    (max_x - min_x) * (max_y - min_y)
}

fn parse_input() -> Vec<Point> {
    include_str!("../../../../inputs/2018/10.txt").lines()
        .map(|line| line.split(|c: char| !c.is_numeric() && c != '-').filter_map(|s| s.parse().ok()).collect_tuple().unwrap())
        .map(|(a, b, c, d)| Point { position: (a, b), velocity: (c, d) })
        .collect()
}

fn ticks_for_smallest_bounding_box(points: &[Point]) -> isize {
    let starting_size = size_after_ticks(points, 0);
    let jump_size = iterate(1, |x| 2 * x).skip_while(|&x| size_after_ticks(points, x) < starting_size).next().unwrap();
    iterate(jump_size, |x| x / 2).take_while(|&x| x > 0).fold(0, |ticks, jump| {
        if size_after_ticks(points, ticks + jump) < size_after_ticks(points, ticks + jump - 1) { ticks + jump } else { ticks }
    })
}

fn find_message(points: &[Point], ticks: isize) -> String {
    let ((min_x, max_x), (min_y, max_y)) = bounding_box(points, ticks);
    let mut message = vec![vec![' '; max_x.abs_diff(min_x) + 1]; max_y.abs_diff(min_y) + 1];
    for (x, y) in points.iter().map(|point| point.moved(ticks)) {
        message[y.abs_diff(min_y)][x.abs_diff(min_x)] = '#';
    }
    message.into_iter().map(|row| row.into_iter().join("")).join("\n")
}

fn main() {
    let start_time = Instant::now();
    let points = parse_input();
    let ticks_for_message = ticks_for_smallest_bounding_box(&points);
    let message = find_message(&points, ticks_for_message);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: \n\n{}\n", message);
    println!("Part 2: {}", ticks_for_message);
    println!("Elapsed: {:?}", elapsed_time);
}
