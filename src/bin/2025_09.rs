use itertools::Itertools;
use std::cmp::{min, max};
use std::time::Instant;

fn orientation(points: &[(isize, isize)]) -> isize {
    let len = points.len();
    let signed_area = points.iter().copied().tuple_windows()
        .map(|((a, b), (c, d))| a * d - b * c)
        .sum::<isize>() + points[len - 1].0 * points[0].1 - points[len - 1].1 * points[0].0;
    signed_area.signum()
}

fn excludes_part_of_rectangle(p1: (isize, isize), p2: (isize, isize), start: (isize, isize), end: (isize, isize), polygon_signum: isize) -> bool {
    let (left, right, bottom, top) = (min(p1.0, p2.0), max(p1.0, p2.0), min(p1.1, p2.1), max(p1.1, p2.1));
    
    // Above the line is outside the polygon
    if (start.0 - end.0).signum() == polygon_signum {
        if bottom <= start.1 + 1 && start.1 + 1 <= top && max(start.0, end.0) > left && min(start.0, end.0) < right {
            return true;
        }
    }

    // Below the line is outside the polygon
    else if (start.0 - end.0).signum() == -polygon_signum {
        if bottom <= start.1 - 1 && start.1 - 1 <= top && max(start.0, end.0) > left && min(start.0, end.0) < right {
            return true;
        }
    }

    // Left of the line is outside the polygon
    else if (start.1 - end.1).signum() == polygon_signum {
        if left <= start.0 - 1 && start.0 - 1 <= right && max(start.1, end.1) > bottom && min(start.1, end.1) < top {
            return true;
        }
    }

    // Right of the line is outside the polygon
    else if (start.1 - end.1).signum() == -polygon_signum {
        if left <= start.0 + 1 && start.0 + 1 <= right && max(start.1, end.1) > bottom && min(start.1, end.1) < top {
            return true;
        }
    }

    false
}

fn is_valid_rectangle(p1: (isize, isize), p2: (isize, isize), polygon_signum: isize, positions: &[(isize, isize)]) -> bool {
    !positions.iter().cycle().take(positions.len()).tuple_windows()
        .any(|(&start, &end)| excludes_part_of_rectangle(p1, p2, start, end, polygon_signum))
}

fn parse_input() -> Vec<(isize, isize)> {
    include_str!("../../inputs/2025/09.txt").lines()
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
    positions.iter().tuple_combinations()
        .filter(|&(&p1, &p2)| is_valid_rectangle(p1, p2, polygon_signum, positions))
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
