use itertools::Itertools;
use std::collections::HashSet;
use std::time::Instant;

struct Example {
    before: [usize; 4],
    after: [usize; 4],
    operation: Operation,
}

struct Args {
    input1: usize,
    input2: usize,
    output: usize,
}

struct Operation {
    opcode: usize,
    args: Args,
}

impl Operation {
    fn from_str(s: &str) -> Self {
        let (opcode, input1, input2, output) = s.split_ascii_whitespace().map(|s| s.parse().unwrap()).collect_tuple().unwrap();
        Self { opcode, args: Args { input1, input2, output } }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum Instruction {
    Addr, Addi,
    Mulr, Muli,
    Banr, Bani,
    Borr, Bori,
    Setr, Seti,
    Gtir, Gtri, Gtrr,
    Eqir, Eqri, Eqrr,
}

use Instruction::*;
const ALL_INSTRUCTIONS: &'static [Instruction; 16] = &[Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr];

fn execute(instruction: Instruction, args: &Args, registers: &[usize; 4]) -> [usize; 4] {
    let mut result = registers.clone();
    result[args.output] = match instruction {
        Addr => result[args.input1] + result[args.input2],
        Addi => result[args.input1] + args.input2,
        Mulr => result[args.input1] * result[args.input2],
        Muli => result[args.input1] * args.input2,
        Banr => result[args.input1] & result[args.input2],
        Bani => result[args.input1] & args.input2,
        Borr => result[args.input1] | result[args.input2],
        Bori => result[args.input1] | args.input2,
        Setr => result[args.input1],
        Seti => args.input1,
        Gtir => if args.input1 > result[args.input2] { 1 } else { 0 },
        Gtri => if result[args.input1] > args.input2 { 1 } else { 0 },
        Gtrr => if result[args.input1] > result[args.input2] { 1 } else { 0 },
        Eqir => if args.input1 == result[args.input2] { 1 } else { 0 },
        Eqri => if result[args.input1] == args.input2 { 1 } else { 0 },
        Eqrr => if result[args.input1] == result[args.input2] { 1 } else { 0 },
    };
    result
}

fn matches(example: &Example, instruction: Instruction) -> bool {
    example.after == execute(instruction, &example.operation.args, &example.before)
}

fn matching_instructions(example: &Example) -> impl Iterator<Item = Instruction> {
    ALL_INSTRUCTIONS.iter().copied().filter(|&it| matches(example, it))
}

fn possible_assignments(examples: &[Example]) -> Vec<HashSet<Instruction>> {
    let mut result = vec![ALL_INSTRUCTIONS.iter().copied().collect(); ALL_INSTRUCTIONS.len()];
    examples.iter().for_each(|example| {
        result[example.operation.opcode] = &result[example.operation.opcode] & &matching_instructions(example).collect()
    });
    result
}

fn find_naked_single(assignments: &[HashSet<Instruction>]) -> Option<(usize, Instruction)> {
    assignments.iter().position(|it| it.len() == 1).map(|idx| (idx, assignments[idx].iter().copied().exactly_one().ok().unwrap()))
}

fn find_hidden_single(assignments: &[HashSet<Instruction>]) -> Option<(usize, Instruction)> {
    ALL_INSTRUCTIONS.iter().copied().filter_map(|instr|
        (0 .. assignments.len()).filter(|&idx| assignments[idx].contains(&instr)).exactly_one().ok().map(|idx| (idx, instr))
    ).next()
}

fn solve_assignments(examples: &[Example]) -> Vec<Instruction> {
    let mut possible_assignments = possible_assignments(examples);
    let mut final_assignments = vec![None; ALL_INSTRUCTIONS.len()];
    for _ in 0 .. ALL_INSTRUCTIONS.len() {
        let (idx, instr) = find_naked_single(&possible_assignments).or_else(|| find_hidden_single(&possible_assignments)).unwrap();
        final_assignments[idx] = Some(instr);
        possible_assignments.iter_mut().for_each(|it| { it.remove(&instr); });
    }
    final_assignments.into_iter().map(|instr| instr.unwrap()).collect()
}

fn parse_input() -> (Vec<Example>, Vec<Operation>) {
    let mut lines = include_str!("../../../../inputs/2018/16.txt").lines().filter(|line| !line.is_empty()).peekable();
    let mut examples = Vec::new();
    while lines.peek().unwrap().starts_with("Before") { 
        let (before, op, after) = lines.next_tuple().unwrap();
        examples.push(Example {
            before: before.split(|c: char| !c.is_numeric()).filter_map(|s| s.parse().ok()).collect_array().unwrap(),
            after: after.split(|c: char| !c.is_numeric()).filter_map(|s| s.parse().ok()).collect_array().unwrap(),
            operation: Operation::from_str(op),
        });
    }
    let operations = lines.map(Operation::from_str).collect_vec();

    (examples, operations)
}

fn part1(examples: &[Example]) -> usize {
    examples.iter().filter(|ex| matching_instructions(ex).count() >= 3).count()
}

fn part2(examples: &[Example], operations: &[Operation]) -> usize {
    let assignments = solve_assignments(examples);
    operations.iter().fold([0; 4], |registers, op| execute(assignments[op.opcode], &op.args, &registers))[0]
}

fn main() {
    let start_time = Instant::now();
    let (examples, operations) = parse_input();
    let part1_ans = part1(&examples);
    let part2_ans = part2(&examples, &operations);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
