use advent_of_code::intcode::IntcodeRunner;
use std::time::Instant;

enum StepResult {
    Work,
    Idle,
}

struct Network {
    runners: Vec<IntcodeRunner>,
    nat: (Option<isize>, Option<isize>, Option<isize>),
}

impl Network {
    fn for_program(n: usize, program: &[isize]) -> Self {
        Self { 
            runners: (0 .. n).map(|idx| {
                let mut runner = IntcodeRunner::for_program(program.to_vec());
                runner.push_input(idx as isize); runner
            }).collect(),
            nat: (None, None, None),
        }
    }

    fn step(&mut self) -> StepResult {
        for idx in 0 .. self.runners.len() {
            self.runners[idx].run();
            match self.runners[idx].pop_output() {
                Some(addr) => {
                    let (x, y) = (self.runners[idx].pop_output(), self.runners[idx].pop_output());
                    if 0 <= addr && (addr as usize) < self.runners.len() { self.runners[addr as usize].push_inputs([x.unwrap(), y.unwrap()]); }
                    if addr == 255 { (self.nat.0, self.nat.1) = (x, y); }
                    return StepResult::Work;
                }
                None => { self.runners[idx].push_input(-1); }
            }
        }
        StepResult::Idle
    }

    fn restart(&mut self) {
        if let (Some(x), Some(y)) = (self.nat.0, self.nat.1) {
            self.runners[0].push_inputs([x, y]);
            self.nat.2 = Some(y);
        }
    }
}

fn parse_input() -> Vec<isize> {
    include_str!("../../../../inputs/2019/23.txt").trim().split(",")
        .map(|s| s.parse().unwrap())
        .collect()
}

fn part1(program: &[isize]) -> isize {
    let mut network = Network::for_program(50, &program);
    while network.nat.1.is_none() { network.step(); }
    network.nat.1.unwrap()
}

fn part2(program: &[isize]) -> isize {
    let mut network = Network::for_program(50, &program);
    loop {
        while matches!(network.step(), StepResult::Work) {}
        if network.nat.1 == network.nat.2 && network.nat.1.is_some() { return network.nat.1.unwrap() }
        network.restart();
    }
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let part1_ans = part1(&program);
    let part2_ans = part2(&program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
