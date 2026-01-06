use itertools::{Itertools, iproduct, iterate};
use std::collections::HashMap;
use std::time::Instant;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum State {
    Sand,
    Clay,
    Reached,
    Settled,
}

fn parse_range(s: &str) -> (isize, isize) {
    s.split("..").map(|x| x.parse().unwrap()).cycle().next_tuple().unwrap()
}

fn parse_line(line: &str) -> ((isize, isize), (isize, isize)) {
    let (r1, r2) = line.split(", ").map(|s| parse_range(&s[2..])).collect_tuple().unwrap();
    if line.starts_with("x") { (r1, r2) } else { (r2, r1) }
}

fn is_free(map: &HashMap<(isize, isize), State>, (y, x): (isize, isize)) -> bool {
    match map.get(&(y, x)).unwrap_or(&State::Sand) {
        State::Sand | State::Reached => true,
        _ => false,
    }
}

fn fill_diagram_from(source: (isize, isize), max_y: isize, map: &mut HashMap<(isize, isize), State>) {
    let mut stack = vec![source];
    while let Some((y, x)) = stack.pop() {
        if y > max_y || !is_free(map, (y, x)) { continue; }
        if !is_free(map, (y + 1, x)) {
            let left = iterate(x, |&l| l - 1).take_while(|&l| is_free(map, (y, l)) && !is_free(map, (y + 1, l))).last().unwrap();
            let right = iterate(x, |&r| r + 1).take_while(|&r| is_free(map, (y, r)) && !is_free(map, (y + 1, r))).last().unwrap();
            if map.get(&(y, left - 1)) == Some(&State::Clay) && map.get(&(y, right + 1)) == Some(&State::Clay) {
                (left ..= right).for_each(|xx| { map.insert((y, xx), State::Settled); });
                stack.push((y - 1, x));
            } else {
                (left ..= right).for_each(|xx| { map.insert((y, xx), State::Reached); });
                if is_free(map, (y + 1, left - 1)) { stack.push((y, left - 1)); }
                if is_free(map, (y + 1, right + 1)) { stack.push((y, right + 1)); }
            }
        } else if !map.contains_key(&(y, x)) {
            map.insert((y, x), State::Reached);
            stack.push((y + 1, x));
        }
    }
}

fn count_states(map: &HashMap<(isize, isize), State>, (min_y, max_y): (isize, isize), valid_states: &[State]) -> usize {
    map.iter().filter(|&(&(y, _x), st)| min_y <= y && y <= max_y && valid_states.contains(st)).count()
}

fn parse_input() -> (HashMap<(isize, isize), State>, (isize, isize)) {
    let map: HashMap<_, _> = include_str!("../../../../inputs/2018/17.txt").lines()
        .map(parse_line)
        .flat_map(|((min_x, max_x), (min_y, max_y))| iproduct!(min_y ..= max_y, min_x ..= max_x))
        .map(|(y, x)| ((y, x), State::Clay))
        .collect();
    let (min_y, max_y) = map.keys().copied().map(|(y, _x)| y).minmax().into_option().unwrap();
    (map, (min_y, max_y))
}

fn main() {
    let start_time = Instant::now();
    let (mut map, (min_y, max_y)) = parse_input();
    fill_diagram_from((0, 500), max_y, &mut map);
    let part1_ans = count_states(&map, (min_y, max_y), &[State::Reached, State::Settled]);
    let part2_ans = count_states(&map, (min_y, max_y), &[State::Settled]);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
