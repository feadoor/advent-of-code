use std::time::Instant;

fn parse_input() -> Vec<usize> {
    include_str!("../../../../inputs/2020/15.txt").split(',').map(|s| s.trim().parse().unwrap()).collect()
}

fn nth_number(starting_nums: &[usize], n: usize) -> usize {
    assert!(starting_nums.len() > 0);
    let (mut last, mut game) = (0, vec![None; n]);
    for idx in 0 .. n {
        let next = match idx {
            idx if idx < starting_nums.len() => starting_nums[idx],
            idx => idx - game[last].unwrap_or(idx),
        };
        if idx > 0 { game[last] = Some(idx); }
        last = next;
    }
    last
}

fn part1(starting_nums: &[usize]) -> usize {
    nth_number(starting_nums, 2020)
}

fn part2(starting_nums: &[usize]) -> usize {
    nth_number(starting_nums, 30_000_000)
}

fn main() {
    let start_time = Instant::now();
    let starting_nums = parse_input();
    let part1_ans = part1(&starting_nums);
    let part2_ans = part2(&starting_nums);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
