use std::time::Instant;

fn sum_of_divisors(n: usize) -> usize {
    let s = ((n as f64).sqrt().floor()) as usize;
    (if s * s == n { s } else { 0 }) + (1 .. s).filter_map(|d| (n % d == 0).then_some(d + n / d)).sum::<usize>()
}

fn part1() -> usize {
    // Value parsed by hand from input file
    sum_of_divisors(986)
}

fn part2() -> usize {
    // Value parsed by hand from input file
    sum_of_divisors(10_551_386)
}

fn main() {
    let start_time = Instant::now();
    let part1_ans = part1();
    let part2_ans = part2();
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
