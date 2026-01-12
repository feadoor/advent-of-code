use std::collections::VecDeque;
use std::iter::once;
use std::time::Instant;

fn power_level(x: isize, y: isize, serial: isize) -> isize {
    let base_power = ((x + 10) * y + serial)* (x + 10);
    (base_power % 1000) / 100 - 5
}

fn compute_grid(serial: isize, size: isize) -> Vec<Vec<isize>> {
    (1 ..= size).map(|y| (1 ..= size).map(|x| power_level(x, y, serial)).collect()).collect()
}

fn row_sums(grid: &Vec<Vec<isize>>) -> Vec<Vec<isize>> {
    grid.iter().map(|row| once(0).chain(row.iter().scan(0, |acc, x| { *acc += x; Some(*acc) })).collect()).collect()
}

fn max_contiguous_sum<I: IntoIterator<Item = isize>>(nums: I, sz: usize) -> (isize, usize) {
    let mut nums = nums.into_iter();
    let mut values: VecDeque<_> = (0 .. sz).map(|_| nums.next().unwrap()).collect();
    let mut acc: isize = values.iter().sum();
    let (mut best, mut best_idx) = (acc, 0);
    
    for (idx, val) in nums.enumerate() {
        acc = acc - values.pop_front().unwrap() + val;
        values.push_back(val);
        if acc > best { (best, best_idx) = (acc, idx + 1); }
    }

    (best, best_idx)
}

fn best_square_of_size(row_sums: &Vec<Vec<isize>>, sz: usize) -> (isize, usize, usize) {
    let (mut best, mut best_y, mut best_x) = (isize::MIN, 0, 0);
    for x in 0 ..= row_sums.len() - sz {
        let (sum, y) = max_contiguous_sum((0 .. row_sums.len()).map(|y| row_sums[y][x + sz] - row_sums[y][x]), sz);
        if sum > best { (best, best_y, best_x) = (sum, y, x); }
    }
    (best, best_y, best_x)
}

fn best_square(row_sums: &Vec<Vec<isize>>) -> (isize, usize, usize, usize) {
    (1 ..= row_sums.len())
        .map(|sz| { let (sum, y, x) = best_square_of_size(row_sums, sz); (sum, y, x, sz) })
        .max().unwrap()
}

fn parse_input() -> isize {
    include_str!("../../../../inputs/2018/11.txt").trim().parse().unwrap()
}

fn part1(row_sums: &Vec<Vec<isize>>) -> (usize, usize) {
    let (_sum, y, x) = best_square_of_size(row_sums, 3);
    (x + 1, y + 1)
}

fn part2(row_sums: &Vec<Vec<isize>>) -> (usize, usize, usize) {
    let (_sum, y, x, sz) = best_square(row_sums);
    (x + 1, y + 1, sz)
}

fn main() {
    let start_time = Instant::now();
    let serial = parse_input();
    let row_sums = row_sums(&compute_grid(serial, 300));
    let part1_ans = part1(&row_sums);
    let part2_ans = part2(&row_sums);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {:?}", part1_ans);
    println!("Part 2: {:?}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
