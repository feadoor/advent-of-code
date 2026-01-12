use itertools::Itertools;
use std::cmp::{min, max};
use std::time::Instant;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Range {
    fixed: isize,
    variable_min: isize,
    variable_max: isize,
}

fn orientation(points: &[(isize, isize)]) -> isize {
    points.iter().copied().cycle().take(points.len()).tuple_windows()
        .map(|((a, b), (c, d))| a * d - b * c)
        .sum::<isize>().signum()
}

fn disallowed_ranges(positions: &[(isize, isize)], polygon_signum: isize) -> (Vec<Range>, Vec<Range>) {
    let (mut horizontal, mut vertical) = (Vec::new(), Vec::new());
    for (start, end) in positions.iter().copied().cycle().take(positions.len()).tuple_windows() {

        // Above the line is outside the polygon
        if (start.0 - end.0).signum() == polygon_signum {
            horizontal.push(Range { fixed: start.1 + 1, variable_min: min(start.0, end.0) + 1, variable_max: max(start.0, end.0) - 1 });
        }

        // Below the line is outside the polygon
        else if (start.0 - end.0).signum() == -polygon_signum {
            horizontal.push(Range { fixed: start.1 - 1, variable_min: min(start.0, end.0) + 1, variable_max: max(start.0, end.0) - 1 });
        }

        // Left of the line is outside the polygon
        else if (start.1 - end.1).signum() == polygon_signum {
            vertical.push(Range { fixed: start.0 - 1, variable_min: min(start.1, end.1) + 1, variable_max: max(start.1, end.1) - 1 });
        }
        
        // Right of the line is outside the polygon
        else if (start.1 - end.1).signum() == -polygon_signum {
            vertical.push(Range { fixed: start.0 + 1, variable_min: min(start.1, end.1) + 1, variable_max: max(start.1, end.1) - 1 });
        }

    }

    horizontal.sort(); vertical.sort();
    (horizontal, vertical)
}

fn is_valid_rectangle(p1: (isize, isize), p2: (isize, isize), horizontal_ranges: &[Range], vertical_ranges: &[Range]) -> bool {
    let (left, right, bottom, top) = (min(p1.0, p2.0), max(p1.0, p2.0), min(p1.1, p2.1), max(p1.1, p2.1));
    
    let horizontal_idx = horizontal_ranges.partition_point(|r| r.fixed < bottom);
    if horizontal_ranges[horizontal_idx..].iter().take_while(|r| r.fixed <= top).any(|r| r.variable_min <= right && r.variable_max >= left) {
        return false;
    }

    let vertical_idx = vertical_ranges.partition_point(|r| r.fixed < left);
    if vertical_ranges[vertical_idx..].iter().take_while(|r| r.fixed <= right).any(|r| r.variable_min <= top && r.variable_max >= bottom) {
        return false;
    }

    true
}

fn parse_input() -> Vec<(isize, isize)> {
    include_str!("../../../../inputs/2025/09.txt").lines()
        .map(|line| line.split(",").map(|s| s.parse().unwrap()).collect_tuple().unwrap())
        .collect()
}

fn part1(positions: &[(isize, isize)]) -> usize {
    positions.iter().tuple_combinations()
        .map(|(&(a, b), &(c, d))| (c.abs_diff(a) + 1) * (d.abs_diff(b) + 1))
        .max().unwrap()
}

fn part2(positions: &[(isize, isize)]) -> usize {
    let polygon_signum = orientation(positions);
    let (horizontal, vertical) = disallowed_ranges(positions, polygon_signum);
    positions.iter().tuple_combinations()
        .filter(|&(&p1, &p2)| is_valid_rectangle(p1, p2, &horizontal, &vertical))
        .map(|(&(a, b), &(c, d))| (c.abs_diff(a) + 1) * (d.abs_diff(b) + 1))
        .max().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let positions = parse_input();
    let part1_ans = part1(&positions);
    let part2_ans = part2(&positions);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
