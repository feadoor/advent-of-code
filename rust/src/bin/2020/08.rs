use std::collections::HashSet;
use std::time::Instant;

enum Instruction {
    Acc(isize),
    Jmp(isize),
    Nop(isize),
}

impl Instruction {
    fn from_str(s: &str) -> Self {
        let (instr, val) = s.split_once(" ").unwrap();
        match instr {
            "acc" => Self::Acc(val.parse().unwrap()),
            "jmp" => Self::Jmp(val.parse().unwrap()),
            "nop" => Self::Nop(val.parse().unwrap()),
            _ => panic!("Unknown instruction {}", instr),
        }
    }

    fn swap_corrupted(&self) -> Option<Self> {
        match self {
            Self::Acc(_) => None,
            &Self::Jmp(val) => Some(Self::Nop(val)),
            &Self::Nop(val) => Some(Self::Jmp(val)),
        }
    }
}

enum ExecutionResult {
    Loop(isize),
    Terminate(isize),
}

fn execute(program: &[Instruction]) -> ExecutionResult {
    let (mut acc, mut pc, mut seen) = (0, 0, HashSet::new());
    loop {
        if !seen.insert(pc) { return ExecutionResult::Loop(acc); }
        if pc < 0 || pc as usize >= program.len() { return ExecutionResult::Terminate(acc); }
        match program[pc as usize] {
            Instruction::Acc(val) => { acc += val; pc += 1; }
            Instruction::Jmp(val) => { pc += val; }
            Instruction::Nop(_) => { pc += 1; }
        }
    }
}

fn parse_input() -> Vec<Instruction> {
    include_str!("../../../../inputs/2020/08.txt").lines().map(Instruction::from_str).collect()
}

fn part1(program: &[Instruction]) -> isize {
    match execute(program) {
        ExecutionResult::Loop(val) => val,
        ExecutionResult::Terminate(_) => panic!("Program was supposed to loop"),
    }
}

fn part2(mut program: Vec<Instruction>) -> isize {
    for idx in 0 .. program.len() {
        if let Some(corrected) = program[idx].swap_corrupted() {
            program[idx] = corrected;
            if let ExecutionResult::Terminate(val) = execute(&program) { return val; }
            program[idx] = program[idx].swap_corrupted().unwrap();
        }
    }
    panic!("Program never terminated");
}

fn main() {
    let start_time = Instant::now();
    let program = parse_input();
    let part1_ans = part1(&program);
    let part2_ans = part2(program);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
