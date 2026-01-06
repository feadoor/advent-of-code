use itertools::{Itertools, iterate};
use std::collections::VecDeque;
use std::time::Instant;

struct RecipeIterator {
    recipes: Vec<usize>,
    elves: (usize, usize),
    yield_idx: usize,
}

impl RecipeIterator {
    fn new() -> Self {
        Self {
            recipes: vec![3, 7],
            elves: (0, 1),
            yield_idx: 0,
        }
    }
}

impl Iterator for RecipeIterator {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.yield_idx == self.recipes.len() {
            let (elf1, elf2) = self.elves;
            let (recipe1, recipe2) = (self.recipes[elf1], self.recipes[elf2]);
            if recipe1 + recipe2 < 10 { self.recipes.push(recipe1 + recipe2); } 
            else { self.recipes.push(1); self.recipes.push(recipe1 + recipe2 - 10); }
            self.elves = ((elf1 + recipe1 + 1) % self.recipes.len(), (elf2 + recipe2 + 1) % self.recipes.len());
        }

        self.yield_idx += 1;
        Some(self.recipes[self.yield_idx - 1])
    }
}

fn parse_input() -> usize {
    include_str!("../../../../inputs/2018/14.txt").trim().parse().unwrap()
}

fn part1(input: usize) -> String {
    RecipeIterator::new().skip(input).take(10).join("")
}

fn part2(input: usize) -> usize {
    let digits = iterate(input, |x| x / 10).take_while(|&x| x > 0).map(|x| x % 10).collect_vec();
    let mut recipes = RecipeIterator::new();
    let mut last_n = VecDeque::new();
    let mut count = 0;
    (0 .. digits.len()).for_each(|_| last_n.push_back(recipes.next().unwrap()));

    loop {
        if last_n.iter().eq(digits.iter().rev()) { return count; }
        last_n.pop_front(); last_n.push_back(recipes.next().unwrap());
        count += 1;
    }
}

fn main() {
    let start_time = Instant::now();
    let input = parse_input();
    let part1_ans = part1(input);
    let part2_ans = part2(input);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
