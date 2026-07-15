use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

#[derive(Clone)]
struct Mask([Option<bool>; 36]);

impl Mask {
    fn empty() -> Self {
        Self([None; 36])
    }

    fn from_str(s: &str) -> Self {
        Self(s.chars().rev().map(|c| match c {
            '0' => Some(false),
            '1' => Some(true),
            _ => None,
        }).collect_array().unwrap())
    }

    fn apply_part1(&self, mut val: usize) -> usize {
        self.0.iter().enumerate().for_each(|(idx, set)|
            match set {
                Some(true) => { val |= 1 << idx; }
                Some(false) => { val &= !(1 << idx); }
                None => {}
            }
        );
        val
    }

    fn apply_part2(&self, mut val: usize) -> Vec<usize> {
        for idx in (0 .. self.0.len()).filter(|&idx| self.0[idx] == Some(true)) {
            val |= 1 << idx;
        }
        let mut results = vec![val];
        for idx in (0 .. self.0.len()).filter(|&idx| self.0[idx] == None) {
            for jdx in 0 .. results.len() {
                results[jdx] &= !(1 << idx);
                results.push(results[jdx] | (1 << idx));
            }
        }
        results
    }
}

enum Command {
    UpdateMask(Mask),
    Write(usize, usize),
}

impl Command {
    fn from_str(s: &str) -> Self {
        if s.starts_with("mask") {
            Self::UpdateMask(Mask::from_str(s.split_once(" = ").unwrap().1))
        } else if s.starts_with("mem") {
            let (dst, val) = s.split_once(" = ").unwrap();
            Self::Write(dst[4 .. dst.len() - 1].parse().unwrap(), val.parse().unwrap())
        }
        else { panic!("Unknown command {}", s) }
    }
}

fn run_part1(program: &[Command]) -> HashMap<usize, usize> {
    let (mut mask, mut memory) = (Mask::empty(), HashMap::new());
    for command in program {
        match command {
            Command::UpdateMask(new_mask) => mask = new_mask.clone(),
            &Command::Write(dst, val) => { memory.insert(dst, mask.apply_part1(val)); }
        }
    }
    memory
}

fn run_part2(program: &[Command]) -> HashMap<usize, usize> {
    let (mut mask, mut memory) = (Mask::empty(), HashMap::new());
    for command in program {
        match command {
            Command::UpdateMask(new_mask) => mask = new_mask.clone(),
            &Command::Write(dst, val) => {
                for dst in mask.apply_part2(dst) { memory.insert(dst, val); }
            }
        }
    }
    memory
}

fn parse_input() -> Vec<Command> {
    include_str!("../../../../inputs/2020/14.txt").lines().map(Command::from_str).collect()
}

fn part1(program: &[Command]) -> usize {
    run_part1(program).values().sum()
}

fn part2(program: &[Command]) -> usize {
    run_part2(program).values().sum()
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
