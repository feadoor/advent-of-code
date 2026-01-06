use advent_of_code::linked_list::LinkedList;
use std::time::Instant;

fn is_match(c1: char, c2: char) -> bool {
    c1.is_ascii_uppercase() != c2.is_ascii_uppercase() && 
    c1.to_ascii_uppercase() == c2.to_ascii_uppercase()
}

fn reacted_length(polymer: &str) -> usize {
    let mut list = LinkedList::from(polymer.chars());
    let mut curr = list.head();
    while let Some((left, right)) = curr.and_then(|it| list.next(&it).map(|next| (it, next))) {
        let (c1, c2) = (*list.val(&left), *list.val(&right));
        if is_match(c1, c2) {
            curr = list.prev(&left).or(list.next(&right));
            list.remove(left);
            list.remove(right);
        } else {
            curr = list.next(&left);
        }
    }
    list.len()
}

fn parse_input() -> &'static str {
    include_str!("../../../../inputs/2018/05.txt").trim()
}

fn part1(polymer: &str) -> usize {
    reacted_length(polymer)
}

fn part2(polymer: &str) -> usize {
    ('A' ..= 'Z')
        .map(|l| polymer.chars().filter(|c| c.to_ascii_uppercase() != l).collect())
        .map(|s: String| reacted_length(&s))
        .min().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let polymer = parse_input();
    let part1_ans = part1(&polymer);
    let part2_ans = part2(&polymer);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
