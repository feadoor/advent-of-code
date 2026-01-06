use advent_of_code::dfs::{DepthFirstSearcher, DepthFirstTraversable, Pruning};
use itertools::{iproduct, Itertools};
use std::cmp::max;
use std::time::Instant;

struct BridgeSearchState {
    components: Vec<[usize; 2]>,
    used_components: Vec<bool>,
    open_pins_stack: Vec<usize>,
    length: usize,
    weight: usize,
}

impl BridgeSearchState {
    fn root(components: Vec<[usize; 2]>) -> Self {
        Self {
            used_components: vec![false; components.len()],
            components,
            open_pins_stack: vec![0],
            length: 0,
            weight: 0,
        }
    }
}

struct BridgeSearchStep {
    component_idx: usize,
    port_idx: usize,
}

struct BridgeSearchOutput {
    length: usize,
    weight: usize,
}

impl DepthFirstTraversable for BridgeSearchState {
    type Step = BridgeSearchStep;
    type Output = BridgeSearchOutput;
    
    fn next_steps(&mut self) -> Vec<Self::Step> {
        let required_pins = *self.open_pins_stack.last().unwrap();
        iproduct!(0 .. self.components.len(), 0 ..= 1)
            .filter(|&(component_idx, port_idx)| !self.used_components[component_idx] && self.components[component_idx][port_idx] == required_pins)
            .map(|(component_idx, port_idx)| BridgeSearchStep { component_idx, port_idx })
            .collect()
    }
    
    fn apply_step(&mut self, &BridgeSearchStep { component_idx, port_idx }: &Self::Step) {
        self.used_components[component_idx] = true;
        self.open_pins_stack.push(self.components[component_idx][1 - port_idx]);
        self.length += 1;
        self.weight += self.components[component_idx][0] + self.components[component_idx][1];
    }
    
    fn revert_step(&mut self, &BridgeSearchStep { component_idx, .. }: &Self::Step) {
        self.used_components[component_idx] = false;
        self.open_pins_stack.pop();
        self.length -= 1;
        self.weight -= self.components[component_idx][0] + self.components[component_idx][1];
    }
    
    fn should_prune(&mut self) -> Pruning {
        Pruning::None
    }
    
    fn output(&mut self) -> Option<Self::Output> {
        if self.next_steps().is_empty() {
            Some(BridgeSearchOutput { length: self.length, weight: self.weight })
        } else {
            None
        }
    }
}

fn max_weights(components: Vec<[usize; 2]>) -> (usize, usize) {
    let (mut max_length, mut max_weight, mut max_weight_of_longest) = (0, 0, 0);
    for BridgeSearchOutput { length, weight } in DepthFirstSearcher::new(BridgeSearchState::root(components)) {
        max_length = max(length, max_length);
        max_weight = max(weight, max_weight);
        if length == max_length { max_weight_of_longest = max(max_weight_of_longest, weight); }
    }

    (max_weight, max_weight_of_longest)
}

fn parse_input() -> Vec<[usize; 2]> {
    include_str!("../../../../inputs/2017/24.txt").lines()
        .map(|line| line.split("/").map(|s| s.parse().unwrap()).collect_array().unwrap())
        .collect()
}

fn main() {
    let start_time = Instant::now();
    let components = parse_input();
    let (part1, part2) = max_weights(components);
    let elapsed_time = start_time.elapsed();
    
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
    println!("Elapsed: {:?}", elapsed_time);
}
