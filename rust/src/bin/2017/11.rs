use std::time::Instant;

fn make_move((a, b, c): (isize, isize, isize), direction: &str) -> (isize, isize, isize) {
    match direction {
        "n" => (a + 1, b, c),
        "s" => (a - 1, b, c),
        "sw" => (a, b + 1, c),
        "ne" => (a, b - 1, c),
        "se" => (a, b, c + 1),
        "nw" => (a, b, c - 1),
        _ => panic!("Unknown direction {}", direction),
    }
}

fn distance((a, b, c): (isize, isize, isize)) -> isize {
    [a, b, c].into_iter().map(|x| (x - a).abs() + (x - b).abs() + (x - c).abs()).min().unwrap()
}

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2017/11.txt").trim()
}

fn part1(steps: &str) -> isize {
    let endpoint = steps.split(",").fold((0, 0, 0), |pos, step| make_move(pos, step));
    distance(endpoint)
}

fn part2(steps: &str) -> isize {
    let positions = steps.split(",").scan((0, 0, 0), |pos, step| {
        *pos = make_move(*pos, step);
        Some(*pos)
    });
    positions.map(distance).max().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let steps = parse_input();
    let part1_ans = part1(&steps);
    let part2_ans = part2(&steps);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
