use itertools::Itertools;
use std::collections::VecDeque;
use std::time::Instant;

struct State {
    pots: VecDeque<bool>,
    start_idx: isize,
}

impl State {
    fn new(pots: VecDeque<bool>) -> Self {
        Self { pots, start_idx: 0 }
    }

    fn tick(&mut self, rules: &[bool]) {
        self.buffer();
        let mut bitmask = 0;
        for idx in 0 .. self.pots.len() {
            bitmask >>= 1;
            if idx + 2 < self.pots.len() && self.pots[idx + 2] { bitmask |= 16; }
            self.pots[idx] = rules[bitmask];
        }
        self.trim()
    }

    fn sum_of_alive_pots(&self) -> isize {
        (self.start_idx..).zip(self.pots.iter().copied()).filter_map(|(idx, pot)| pot.then_some(idx)).sum()
    }

    fn buffer(&mut self) {
        while self.pots.iter().take(2).any(|&x| x) {
            self.pots.push_front(false);
            self.start_idx -= 1;
        }

        while self.pots.iter().rev().take(2).any(|&x| x) {
            self.pots.push_back(false);
        }
    }

    fn trim(&mut self) {
        while self.pots.iter().take(2).all(|&x| !x) {
            self.pots.pop_front();
            self.start_idx += 1;
        }

        while self.pots.iter().rev().take(2).all(|&x| !x) {
            self.pots.pop_back();
        }
    }
}

fn parse_input() -> (VecDeque<bool>, Vec<bool>) {
    let mut lines = include_str!("../../../../inputs/2018/12.txt").lines();
    let initial_state = lines.next().unwrap().split_once(": ").unwrap().1.chars().map(|c| c == '#').collect();

    let mut transitions = vec![false; 32];
    for transition in lines.skip(1) {
        let (from, to) = transition.split_once(" => ").unwrap();
        let from: usize = from.chars().enumerate().map(|(idx, c)| if c == '#' { 1 << idx } else { 0 }).sum();
        transitions[from] = to == "#";
    }

    (initial_state, transitions)
}

fn part1(initial_state: VecDeque<bool>, transitions: &[bool]) -> isize {
    let mut state = State::new(initial_state);
    (0 .. 20).for_each(|_| state.tick(transitions));
    state.sum_of_alive_pots()
}

fn part2(initial_state: VecDeque<bool>, transitions: &[bool]) -> isize {
    let (mut state, mut differences) = (State::new(initial_state), VecDeque::new());
    let (mut ticks, mut total) = (0, state.sum_of_alive_pots());
    
    while ticks < 50_000_000 && (differences.len() < 100 || !differences.iter().all_equal()) {
        state.tick(transitions);
        ticks += 1;
        let new_total = state.sum_of_alive_pots();
        differences.push_back(new_total - total);
        if differences.len() > 100 { differences.pop_front(); }
        total = new_total;
    }

    let (difference, ticks_remaining) = (differences.back().unwrap(), 50_000_000 - ticks);
    total + ticks_remaining * difference
}

fn main() {
    let start_time = Instant::now();
    let (initial_state, transitions) = parse_input();
    let part1_ans = part1(initial_state.clone(), &transitions);
    let part2_ans = part2(initial_state, &transitions);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
