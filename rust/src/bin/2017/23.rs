use std::time::Instant;

fn is_prime(n: usize) -> bool {
    (2 ..).take_while(|&d| d * d <= n).all(|d| n % d != 0)
}

fn part1() -> usize {
    // Value of b parsed by hand from input file
    let b = 84;
    (b - 2) * (b - 2)
}

fn part2() -> usize {
    // Values of b, c and step parsed by hand from input file
    let (b, c, step) = (108_400, 125_400, 17);
    (b ..= c).step_by(step).filter(|&n| !is_prime(n)).count()
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
