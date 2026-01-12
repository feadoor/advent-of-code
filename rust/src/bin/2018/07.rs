use advent_of_code::dag::{Dag, lexmin_toposort, reversed};
use itertools::Itertools;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::time::Instant;

fn time_for_task(task: char) -> usize {
    task as usize - 'A' as usize + 61
}

fn parse_input() -> Dag<char> {
    let lines = include_str!("../../../../inputs/2018/07.txt").lines();
    let mut result: Dag<_> = HashMap::new();
    for line in lines {
        let (c1, c2) = line.split(" ").filter_map(|s| s.chars().exactly_one().ok()).collect_tuple().unwrap();
        result.entry(c1).or_default().insert(c2);
    }
    result
}

fn part1(dag: &Dag<char>) -> String {
    lexmin_toposort(dag).into_iter().join("")
}

fn part2(dag: &Dag<char>) -> usize {
    let mut dependencies = reversed(dag);
    let mut time = 0;
    let mut workers = vec![None; 5];
    let mut jobs: BinaryHeap<_> = dag.keys().filter(|k| !dependencies.contains_key(k)).map(|&k| Reverse(k)).collect();

    while jobs.len() > 0 || workers.iter().any(|w| w.is_some()) {

        for worker in workers.iter_mut() {
            if let Some((job, time)) = worker.take() {
                if time > 1 { *worker = Some((job, time - 1)); }
                else {
                    for dependent in dag.get(&job).into_iter().flatten() {
                        let in_edges = dependencies.get_mut(dependent).unwrap();
                        in_edges.remove(&job);
                        if in_edges.is_empty() { jobs.push(Reverse(*dependent)); }
                    }
                }
            }
        }
        
        for worker in workers.iter_mut().filter(|w| w.is_none()) {
            if let Some(Reverse(job)) = jobs.pop() {
                *worker = Some((job, time_for_task(job)));
            } else {
                break;
            }
        }

        time += 1;
    }

    time - 1
}

fn main() {
    let start_time = Instant::now();
    let dag = parse_input();
    let part1_ans = part1(&dag);
    let part2_ans = part2(&dag);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
