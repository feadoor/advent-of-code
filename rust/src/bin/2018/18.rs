use advent_of_code::grid::eight_neighbours;
use itertools::Itertools;
use std::collections::HashMap; 
use std::time::Instant;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum State {
    Open,
    Tree,
    Lumberyard,
}

impl State {
    fn from_char(c: char) -> Self {
        match c {
            '.' => Self::Open,
            '|' => Self::Tree,
            '#' => Self::Lumberyard,
            _ => panic!("Unknown character {}", c),
        }
    }
}

fn tick(diagram: &Vec<Vec<State>>) -> Vec<Vec<State>> {
    diagram.iter().enumerate().map(|(y, row)| row.iter().enumerate().map(|(x, state)| {
        let nbr_counts = eight_neighbours((y, x), diagram).into_iter().map(|(b, a)| diagram[b][a]).counts();
        match state {
            State::Open => if *nbr_counts.get(&State::Tree).unwrap_or(&0) >= 3 { State::Tree } else { State::Open },
            State::Tree => if *nbr_counts.get(&State::Lumberyard).unwrap_or(&0) >= 3 { State::Lumberyard } else { State::Tree },
            State::Lumberyard => if *nbr_counts.get(&State::Lumberyard).unwrap_or(&0) >= 1 && *nbr_counts.get(&State::Tree).unwrap_or(&0) >= 1 { State::Lumberyard } else { State::Open },
        }
    }).collect()).collect()
}

fn state_after(n: usize, diagram: &Vec<Vec<State>>) -> Vec<Vec<State>> {
    let (mut diagram, mut diagrams) = (diagram.to_owned(), vec![diagram.to_owned()]);
    let (mut seen, mut ticks) = (HashMap::from([(diagram.to_owned(), 0)]), 0);
    loop {
        (diagram, ticks) = (tick(&diagram), ticks + 1);
        if ticks == n { return diagram; }
        else if !seen.contains_key(&diagram) {
            diagrams.push(diagram.clone());
            seen.insert(diagram.clone(), ticks);
        } else {
            let period = ticks - seen[&diagram];
            let index = ticks + ((n - ticks) % period) - period;
            return diagrams[index].to_owned()
        }
    }
}

fn score(diagram: &Vec<Vec<State>>) -> usize {
    let counts = diagram.iter().flatten().counts();
    counts[&State::Lumberyard] * counts[&State::Tree]
}

fn parse_input() -> Vec<Vec<State>> {
    include_str!("../../../../inputs/2018/18.txt").lines()
        .map(|line| line.chars().map(State::from_char).collect())
        .collect()
}

fn part1(diagram: &Vec<Vec<State>>) -> usize {
    score(&state_after(10 , diagram))
}

fn part2(diagram: &Vec<Vec<State>>) -> usize {
    score(&state_after(1_000_000_000, diagram))
}

fn main() {
    let start_time = Instant::now();
    let diagram = parse_input();
    let part1_ans = part1(&diagram);
    let part2_ans = part2(&diagram);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
