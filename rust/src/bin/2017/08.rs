use itertools::Itertools;
use std::cmp::max;
use std::collections::HashMap;
use std::time::Instant;

enum Condition {
    Gt(String, isize),
    Gte(String, isize),
    Lt(String, isize),
    Lte(String, isize),
    Eq(String, isize),
    Neq(String, isize),
}

impl Condition {
    fn from_str(s: &str) -> Self {
        let (name, symbol, value) = s.split_ascii_whitespace().collect_tuple().unwrap();
        match symbol {
            ">" => Self::Gt(name.to_owned(), value.parse().unwrap()),
            ">=" => Self::Gte(name.to_owned(), value.parse().unwrap()),
            "<" => Self::Lt(name.to_owned(), value.parse().unwrap()),
            "<=" => Self::Lte(name.to_owned(), value.parse().unwrap()),
            "==" => Self::Eq(name.to_owned(), value.parse().unwrap()),
            "!=" => Self::Neq(name.to_owned(), value.parse().unwrap()),
            s => panic!("Bad symbol {} in condition", s),
        }
    }
}

enum Command {
    Inc(String, isize, Condition),
    Dec(String, isize, Condition),
}

impl Command {
    fn from_str(s: &str) -> Self {
        let (command, condition) = s.split("if").map(|s| s.trim()).collect_tuple().unwrap();
        let (name, directive, value) = command.split_ascii_whitespace().collect_tuple().unwrap();
        match directive {
            "inc" => Self::Inc(name.to_owned(), value.parse().unwrap(), Condition::from_str(condition)),
            "dec" => Self::Dec(name.to_owned(), value.parse().unwrap(), Condition::from_str(condition)),
            d => panic!("Bad directive {} in command", d),
        }
    }

    fn condition(&self) -> &Condition {
        match self {
            Self::Inc(_, _, c) => &c,
            Self::Dec(_, _, c) => &c,
        }
    }
}

struct Executor {
    memory: HashMap<String, isize>,
}

impl Executor {

    fn new() -> Self {
        Self { memory: HashMap::new() }
    }

    fn get_register(&self, name: &str) -> &isize {
        self.memory.get(name).unwrap_or(&0)
    }

    fn get_register_mut(&mut self, name: &str) -> &mut isize {
        self.memory.entry(name.to_owned()).or_insert(0)
    }

    fn inc(&mut self, name: &str, value: isize) -> isize {
        let register = self.get_register_mut(name);
        *register += value;
        *register
    }

    fn dec(&mut self, name: &str, value: isize) -> isize {
        let register = self.get_register_mut(name);
        *register -= value;
        *register
    }

    fn execute_command(&mut self, command: &Command) -> Option<isize> {
        let condition_met = match command.condition() {
            Condition::Gt(name, value) => self.get_register(name) > value,
            Condition::Gte(name, value) => self.get_register(name) >= value,
            Condition::Lt(name, value) => self.get_register(name) < value,
            Condition::Lte(name, value) => self.get_register(name) <= value,
            Condition::Eq(name, value) => self.get_register(name) == value,
            Condition::Neq(name, value) => self.get_register(name) != value,
        };

        if condition_met {
            match command {
                Command::Inc(name, value, _) => Some(self.inc(name, *value)),
                Command::Dec(name, value, _) => Some(self.dec(name, *value)),
            }
        } else {
            None
        }
    }

    fn max_register_value(&self) -> isize {
        *self.memory.values().max().unwrap()
    }
}

fn parse_input() -> Vec<Command> {
    include_str!("../../../../inputs/2017/08.txt").lines().map(Command::from_str).collect()
}

fn part1(program: &[Command]) -> isize {
    let mut executor = Executor::new();
    for command in program {
        executor.execute_command(command);
    }
    executor.max_register_value()
}

fn part2(program: &[Command]) -> isize {
    let (mut executor, mut max_value) = (Executor::new(), isize::MIN);
    for command in program {
        if let Some(result) = executor.execute_command(command) {
            max_value = max(max_value, result);
        }
    }
    max_value
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(&input);
    let part2_ans = part2(&input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
