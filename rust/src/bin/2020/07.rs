use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::time::Instant;

fn parse_inner_bag(s: &str) -> (usize, &str) {
    let (amt, rest) = s.split_once(" ").unwrap();
    let (name, _) = rest.split_once(" bag").unwrap();
    (amt.parse().unwrap(), name)
}

fn parse_bag_contents(s: &str) -> (&str, Vec<(usize, &str)>) {
    let (name, contents) = s.split_once(" bags contain ").unwrap();
    if contents.starts_with("no other") { (name, Vec::new()) }
    else { (name, contents.split(", ").map(parse_inner_bag).collect()) }
}

fn total_count<'a>(bags: &HashMap<&'a str, Vec<(usize, &'a str)>>, bag: &'a str, memo: &mut HashMap<&'a str, usize>) -> usize {
    if let Some(&result) = memo.get(bag) { result }
    else {
        let result = bags.get(bag).into_iter().flatten().map(|&(amt, name)| amt + amt * total_count(bags, name, memo)).sum();
        memo.insert(bag, result); result
    }
} 

fn parse_input() -> HashMap<&'static str, Vec<(usize, &'static str)>> {
    include_str!("../../../../inputs/2020/07.txt").lines().map(parse_bag_contents).collect()
}

fn part1(bags: &HashMap<&str, Vec<(usize, &str)>>) -> usize {
    let reversed = bags.iter()
        .flat_map(|(&outer, inners)| inners.iter().map(move |&(_amt, name)| (name, outer)))
        .into_group_map();
    let (mut stack, mut seen) = (vec!["shiny gold"], HashSet::from(["shiny gold"]));
    while let Some(bag) = stack.pop() {
        if let Some(outers) = reversed.get(bag) {
            outers.iter().for_each(|&outer| if seen.insert(outer) { stack.push(outer); });
        }
    }
    seen.len() - 1
}

fn part2(bags: &HashMap<&str, Vec<(usize, &str)>>) -> usize {
    let mut memo = HashMap::new();
    total_count(bags, "shiny gold", &mut memo)
}

fn main() {
    let start_time = Instant::now();
    let bags = parse_input();
    let part1_ans = part1(&bags);
    let part2_ans = part2(&bags);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
