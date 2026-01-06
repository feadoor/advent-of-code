use std::time::Instant;

fn spinlock(steps: usize, step_size: usize) -> (Vec<usize>, usize) {
    let (mut result, mut position) = (vec![0], 0);
    for step in 1 ..= steps {
        position = (position + step_size) % result.len() + 1;
        result.insert(position, step);
    }
    (result, position)
}

fn spinlock_value_after_zero(steps: usize, step_size: usize) -> usize {
    let (mut ans, mut position) = (0, 0);
    for step in 1 ..= steps {
        position = (position + step_size) % step + 1;
        if position == 1 { ans = step; }
    }
    ans
}

fn parse_input() -> usize {
    include_str!("../../../../inputs/2017/17.txt").trim().parse().unwrap()
}

fn part1(step_size: usize) -> usize {
    let (lock, position) = spinlock(2017, step_size);
    lock[(position + 1) % lock.len()]
}

fn part2(step_size: usize) -> usize {
    spinlock_value_after_zero(50_000_000, step_size)
}

fn main() {
    let start_time = Instant::now();
    let step_size = parse_input();
    let part1_ans = part1(step_size);
    let part2_ans = part2(step_size);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
