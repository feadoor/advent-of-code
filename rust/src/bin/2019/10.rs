use advent_of_code::math::gcd;
use advent_of_code::more_itertools::InterleaveAllExt;
use itertools::Itertools;
use ordered_float::OrderedFloat;
use std::time::Instant;

fn displacement(src: (isize, isize), dst: (isize, isize)) -> (isize, isize) {
    (dst.0 - src.0, dst.1 - src.1)
}

fn gradient((dy, dx): (isize, isize)) -> (isize, isize) {
    let g = gcd(dy, dx).abs();
    (dy / g, dx / g)
}

fn angle((dy, dx): (isize, isize)) -> f64 {
    let raw_angle = (dx as f64).atan2(dy as f64);
    std::f64::consts::PI - raw_angle
}

fn parse_input() -> Vec<(isize, isize)> {
    include_str!("../../../../inputs/2019/10.txt").lines().enumerate()
        .flat_map(|(row, line)| line.chars().enumerate().filter_map(move |(col, c)|
            (c == '#').then_some((row as isize, col as isize))
        ))
        .collect()
}

fn part1(asteroids: &[(isize, isize)]) -> (usize, (isize, isize)) {
    asteroids.iter().map(|&src| 
        (asteroids.iter().filter_map(|&dst| (src != dst).then(|| gradient(displacement(src, dst)))).unique().count(), src)
    ).max().unwrap()
}

fn part2(asteroids: &[(isize, isize)], src: (isize, isize)) -> isize {
    let deltas = asteroids.iter().filter_map(|&dst| (src != dst).then(|| displacement(src, dst))).sorted_by_key(|(dy, dx)| (dy.abs(), dx.abs()));
    let by_gradient = deltas.into_group_map_by(|&d| gradient(d));
    let ordered_groups = by_gradient.iter().sorted_by_key(|&(&k, _v)| OrderedFloat(angle(k))).map(|(_k, v)| v.into_iter());
    ordered_groups.interleave_all().nth(199).map(|&(y, x)| 100 * (src.1 + x) + (src.0 + y)).unwrap()
}

fn main() {
    let start_time = Instant::now();
    let asteroids = parse_input();
    let (part1_ans, station) = part1(&asteroids);
    let part2_ans = part2(&asteroids, station);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
