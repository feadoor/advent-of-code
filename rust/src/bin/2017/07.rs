use itertools::Itertools;
use regex::Regex;
use std::collections::HashMap;
use std::sync::LazyLock;
use std::time::Instant;

struct InputLine {
    name: String,
    weight: usize,
    children: Vec<String>,
}

impl InputLine {
    fn from_str(s: &str) -> Self {

        static NODE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
            r"(?<name>[a-z]+) \((?<weight>[0-9]+)\)"
        ).unwrap());

        let mut parts = s.split("->").map(|s| s.trim());
        let parent = parts.next().unwrap();
        let captures = NODE_RE.captures(parent).unwrap();
        let children = parts.next().map(|s| s.split(", ").map(|s| s.to_owned()).collect()).unwrap_or_default();

        Self {
            name: captures["name"].to_owned(),
            weight: captures["weight"].parse().unwrap(),
            children,
        }
    }
}

struct Node {
    name: String,
    weight: usize,
    parent: Option<usize>,
    children: Vec<usize>,
}

impl Node {
    fn new(name: String, weight: usize) -> Self {
        Self { name, weight, parent: None, children: Vec::new() }
    }
}

fn root_idx(tree: &[Node]) -> usize {
    let mut idx = 0;
    while let Some(parent) = tree[idx].parent {
        idx = parent;
    }
    idx
}

fn total_weights(tree: &[Node]) -> Vec<usize> {
    let mut weights = vec![0; tree.len()];
    let mut nodes_to_visit = vec![root_idx(tree)];
    while let Some(idx) = nodes_to_visit.pop() {
        if tree[idx].children.iter().all(|&c| weights[c] != 0) {
            weights[idx] = tree[idx].weight + tree[idx].children.iter().map(|&c| weights[c]).sum::<usize>();
        } else {
            nodes_to_visit.push(idx);
            nodes_to_visit.extend_from_slice(&tree[idx].children);
        }
    }
    weights
}

fn parse_input() -> Vec<Node> {
    let lines: Vec<_> = include_str!("../../../../inputs/2017/07.txt").lines().map(InputLine::from_str).collect();
    let lookup: HashMap<_, _> = lines.iter().enumerate().map(|(idx, line)| (line.name.clone(), idx)).collect();
    let mut nodes: Vec<_> = lines.iter().map(|line| Node::new(line.name.clone(), line.weight)).collect();

    for line in lines {
        let parent_idx = lookup[&line.name];
        for child in line.children {
            let child_idx = lookup[&child];
            nodes[parent_idx].children.push(child_idx);
            nodes[child_idx].parent = Some(parent_idx);
        }
    }

    nodes
}

fn part1(tree: &[Node]) -> String {
    tree[root_idx(tree)].name.clone()
}

fn part2(tree: &[Node]) -> usize {
    let weights = total_weights(tree);
    let mut unbalanced_node = root_idx(tree);
    while let Some(&next_idx) = tree[unbalanced_node].children.iter().find(|&&idx| !tree[idx].children.iter().map(|&c| weights[c]).all_equal()) {
        unbalanced_node = next_idx;
    }

    let children = &tree[unbalanced_node].children;
    let bad_child = *children.iter().find(|&&c| children.iter().filter(|&&d| weights[c] == weights[d]).count() == 1).unwrap();
    let good_child = *children.iter().find(|&&c| weights[c] != weights[bad_child]).unwrap();

    tree[bad_child].weight + weights[good_child] - weights[bad_child]
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
