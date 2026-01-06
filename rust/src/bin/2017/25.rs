use regex::Regex;
use std::sync::LazyLock;
use std::collections::HashMap;
use std::time::Instant;

struct Rule {
    write_value: bool,
    move_offset: isize,
    next_state: usize,
}

struct State {
    if_false: Rule,
    if_true: Rule,
}

struct TuringMachine {
    tape: HashMap<isize, bool>,
    cursor: isize,
    state: usize,
    states: Vec<State>,
}

impl TuringMachine {
    fn new(start_state: usize, states: Vec<State>) -> Self {
        Self {
            states,
            state: start_state,
            cursor: 0,
            tape: HashMap::new(),
        }
    }

    fn read_value(&mut self) -> bool {
        *self.tape.get(&self.cursor).unwrap_or(&false)
    }

    fn write_value(&mut self, val: bool) {
        self.tape.insert(self.cursor, val);
    }

    fn step(&mut self) {
        let current_value = self.read_value();
        let &Rule { write_value, move_offset, next_state } = if current_value { &self.states[self.state].if_true } else { &self.states[self.state].if_false };
        self.write_value(write_value);
        self.cursor += move_offset;
        self.state = next_state;
    }
}

fn parse_state_idx(line: &str) -> usize {
    static STATE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
        r"state ([A-Z])"
    ).unwrap());
    (STATE_RE.captures(line).unwrap()[1].bytes().next().unwrap() - b'A') as usize
}

fn parse_steps(line: &str) -> usize {
    static STEPS_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
        r"([0-9]+) steps"
    ).unwrap());
    STEPS_RE.captures(line).unwrap()[1].parse().unwrap()
}

fn parse_write_value(line: &str) -> bool {
    line.contains("1")
}

fn parse_move_offset(line: &str) -> isize {
    if line.contains("left") { -1 } else { 1 }
}

fn parse_input() -> (usize, usize, Vec<State>) {
    let mut lines = include_str!("../../../../inputs/2017/25.txt").lines().filter(|&line| !line.trim().is_empty());
    let start_state = parse_state_idx(lines.next().unwrap());
    let required_steps = parse_steps(lines.next().unwrap());
    let mut states = Vec::new();
    while let Some(_state_line) = lines.next() {
        let _false_line = lines.next();
        let write_value = parse_write_value(lines.next().unwrap());
        let move_offset = parse_move_offset(lines.next().unwrap());
        let next_state = parse_state_idx(lines.next().unwrap());
        let if_false = Rule { write_value, move_offset, next_state };

        let _true_line = lines.next();
        let write_value = parse_write_value(lines.next().unwrap());
        let move_offset = parse_move_offset(lines.next().unwrap());
        let next_state = parse_state_idx(lines.next().unwrap());
        let if_true = Rule { write_value, move_offset, next_state };

        states.push(State { if_false, if_true });
    }

    (start_state, required_steps, states)
}

fn part1(start_state: usize, required_steps: usize, states: Vec<State>) -> usize {
    let mut machine = TuringMachine::new(start_state, states);
    for _ in 0 .. required_steps {
        machine.step();
    }
    machine.tape.values().filter(|&&v| v).count()
}

fn main() {
    let start_time = Instant::now();
    let (start_state, required_steps, states) = parse_input();
    let part1_ans = part1(start_state, required_steps, states);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", "Congratulations!");
    println!("Elapsed: {:?}", elapsed_time);
}
