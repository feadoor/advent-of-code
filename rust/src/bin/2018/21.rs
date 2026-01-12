use itertools::iterate;
use std::collections::HashSet;
use std::time::Instant;

fn mutilate((mut r2, mut r5): (usize, usize)) -> (usize, usize) {
    r2 += r5 & 255;
    r2 &= 16_777_215;
    r2 *= 65_899;
    r2 &= 16_777_215;
    r5 /= 256;
    (r2, r5)
}

fn halting_values() -> impl Iterator<Item = usize> {
    let mut seen = HashSet::new();
    iterate((2_238_642, 65536), |&(r2, r5)| if r5 > 0 { mutilate((r2, r5)) } else { (2_238_642, r2 | 65536)})
        .filter_map(|(r2, r5)| (r5 == 0).then_some(r2))
        .take_while(move |&r2| seen.insert(r2))
}

fn both_parts() -> (usize, usize) {
    let mut values = halting_values();
    (values.next().unwrap(), values.last().unwrap())
}

fn main() {
    let start_time = Instant::now();
    let (part1_ans, part2_ans) = both_parts();
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
