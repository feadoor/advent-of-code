use std::time::Instant;

fn count_stats(s: &str) -> (usize, usize) {
    let (mut score, mut total_garbage) = (0, 0);
    let (mut depth, mut garbage, mut ignore) = (0, false, false);

    for c in s.chars() {
        if ignore { ignore = false; }
        else if garbage {
            match c {
                '>' => garbage = false,
                '!' => ignore = true,
                _ => total_garbage += 1,
            }
        } else {
            match c {
                '{' => { depth += 1; score += depth; },
                '}' => depth -= 1,
                '<' => garbage = true,
                _ => {},
            }
        }
    }

    (score, total_garbage)
}

fn parse_input() -> (usize, usize) {
    count_stats(include_str!("../../../../inputs/2017/09.txt").trim())
}

fn main() {
    let start_time = Instant::now();
    let (score, total_garbage) = parse_input();
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", score);
    println!("Part 2: {}", total_garbage);
    println!("Elapsed: {:?}", elapsed_time);
}
