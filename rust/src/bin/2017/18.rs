use std::collections::{HashMap, VecDeque};
use std::time::Instant;

enum Value {
    Register(String),
    Direct(isize),
}

impl Value {
    fn from_str(s: &str) -> Self {
        if s.chars().all(|c| c.is_alphabetic()) {
            Self::Register(s.to_owned())
        } else {
            Self::Direct(s.parse().unwrap())
        }
    }
}

enum Instruction {
    Snd(Value),
    Set(String, Value),
    Add(String, Value),
    Mul(String, Value),
    Mod(String, Value),
    Rcv(String),
    Jgz(Value, Value),
}

impl Instruction {
    fn from_str(s: &str) -> Self {
        let mut words = s.split_ascii_whitespace();
        match words.next().unwrap() {
            "snd" => Instruction::Snd(Value::from_str(words.next().unwrap())),
            "set" => Instruction::Set(words.next().unwrap().to_owned(), Value::from_str(words.next().unwrap())),
            "add" => Instruction::Add(words.next().unwrap().to_owned(), Value::from_str(words.next().unwrap())),
            "mul" => Instruction::Mul(words.next().unwrap().to_owned(), Value::from_str(words.next().unwrap())),
            "mod" => Instruction::Mod(words.next().unwrap().to_owned(), Value::from_str(words.next().unwrap())),
            "rcv" => Instruction::Rcv(words.next().unwrap().to_owned()),
            "jgz" => Instruction::Jgz(Value::from_str(words.next().unwrap()), Value::from_str(words.next().unwrap())),
            x => panic!("Unknown instruction {}", x),
        }
    }
}

struct SingleRunner<'a> {
    program: &'a [Instruction],
    instruction: isize,
    registers: HashMap<String, isize>,
    last_frequency: Option<isize>,
}

impl<'a> SingleRunner<'a> {
    fn new(program: &'a [Instruction]) -> Self {
        Self {
            program,
            instruction: 0,
            registers: HashMap::new(),
            last_frequency: None,
        }
    }

    fn get_register(&self, name: &str) -> &isize {
        self.registers.get(name).unwrap_or(&0)
    }

    fn get_register_mut(&mut self, name: &str) -> &mut isize {
        self.registers.entry(name.to_owned()).or_insert(0)
    }

    fn get_value(&self, value: &Value) -> isize {
        match value {
            Value::Register(name) => *self.get_register(name),
            Value::Direct(val) => *val,
        }
    }

    fn in_bounds(&self) -> bool {
        self.instruction >= 0 && self.instruction < self.program.len() as isize
    }

    fn step(&mut self) -> Option<isize> {
        use Instruction::*;
        if self.in_bounds() {
            let prev_instruction = self.instruction;
            let result = match &self.program[self.instruction as usize] {
                Snd(val) => { self.last_frequency = Some(self.get_value(val)); None },
                Set(reg, val) => { *self.get_register_mut(reg) = self.get_value(val); None },
                Add(reg, val) => { *self.get_register_mut(reg) += self.get_value(val); None },
                Mul(reg, val) => { *self.get_register_mut(reg) *= self.get_value(val); None },
                Mod(reg, val) => { *self.get_register_mut(reg) %= self.get_value(val); None },
                Rcv(reg) => if *self.get_register(reg) != 0 { self.last_frequency } else { None },
                Jgz(val1, val2) => { if self.get_value(val1) > 0 { self.instruction += self.get_value(val2); } None },
            };
            if self.instruction == prev_instruction {
                self.instruction += 1;
            }
            result
        } else {
            None
        }
    }
}

struct Duettist<'a> {
    program: &'a [Instruction],
    instruction: isize,
    registers: HashMap<String, isize>,
    value_queue: VecDeque<isize>,
    snd_count: usize,
}

impl<'a> Duettist<'a> {
    fn new(program_id: isize, program: &'a [Instruction]) -> Self {
        Self {
            program,
            instruction: 0,
            registers: HashMap::from([("p".to_owned(), program_id)]),
            value_queue: VecDeque::new(),
            snd_count: 0,
        }
    }

    fn get_register(&self, name: &str) -> &isize {
        self.registers.get(name).unwrap_or(&0)
    }

    fn get_register_mut(&mut self, name: &str) -> &mut isize {
        self.registers.entry(name.to_owned()).or_insert(0)
    }

    fn get_value(&self, value: &Value) -> isize {
        match value {
            Value::Register(name) => *self.get_register(name),
            Value::Direct(val) => *val,
        }
    }

    fn in_bounds(&self) -> bool {
        self.instruction >= 0 && self.instruction < self.program.len() as isize
    }

    fn step(&mut self) -> Result<Option<isize>, ()> {
        use Instruction::*;
        if self.in_bounds() {
            let prev_instruction = self.instruction;
            let result = match &self.program[self.instruction as usize] {
                Snd(val) => { self.snd_count += 1; Some(self.get_value(val)) },
                Set(reg, val) => { *self.get_register_mut(reg) = self.get_value(val); None },
                Add(reg, val) => { *self.get_register_mut(reg) += self.get_value(val); None },
                Mul(reg, val) => { *self.get_register_mut(reg) *= self.get_value(val); None },
                Mod(reg, val) => { *self.get_register_mut(reg) %= self.get_value(val); None },
                Rcv(reg) => if let Some(other) = self.value_queue.pop_front() { *self.get_register_mut(reg) = other; None } else { return Err(()); },
                Jgz(val1, val2) => { if self.get_value(val1) > 0 { self.instruction += self.get_value(val2); } None },
            };
            if self.instruction == prev_instruction {
                self.instruction += 1;
            }
            Ok(result)
        } else {
            Err(())
        }
    }
}

struct DoubleRunner<'a> {
    duettists: (Duettist<'a>, Duettist<'a>),
}

impl<'a> DoubleRunner<'a> {
    fn new(program: &'a [Instruction]) -> Self {
        DoubleRunner {
            duettists: (Duettist::new(0, program), Duettist::new(1, program)),
        }
    }

    fn step(&mut self) -> Result<(), ()> {
        let (res1, res2) = (self.duettists.0.step(), self.duettists.1.step());
        if res1.is_err() && res2.is_err() { 
            Err(()) 
        } else {
            if let Ok(Some(x)) = res1 { self.duettists.1.value_queue.push_back(x); }
            if let Ok(Some(x)) = res2 { self.duettists.0.value_queue.push_back(x); }
            Ok(())
        }
    }
}

fn parse_input() -> Vec<Instruction> {
    include_str!("../../../../inputs/2017/18.txt").lines().map(Instruction::from_str).collect()
}

fn part1(program: &[Instruction]) -> isize {
    let mut runner = SingleRunner::new(program);
    loop {
        if let Some(val) = runner.step() {
            return val;
        }
    }
}

fn part2(program: &[Instruction]) -> usize {
    let mut runner = DoubleRunner::new(program);
    while let Ok(()) = runner.step() { }
    runner.duettists.1.snd_count
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
