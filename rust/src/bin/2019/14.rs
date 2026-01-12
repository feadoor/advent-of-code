use advent_of_code::dag::lexmin_toposort;
use itertools::{Itertools, iterate};
use std::collections::{HashMap, HashSet};
use std::time::Instant;

#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Ingredient<'a>(usize, &'a str);

impl<'a> Ingredient<'a> {
    fn from_str(s: &'a str) -> Self {
        let (amount, name) = s.split_once(" ").unwrap();
        Self(amount.parse().unwrap(), name)
    }
}

struct Recipe<'a>(Ingredient<'a>, HashSet<Ingredient<'a>>);

impl<'a> Recipe<'a> {
    fn from_str(s: &'a str) -> Self {
        let (inputs, output) = s.split_once(" => ").unwrap();
        Self(Ingredient::from_str(output), inputs.split(", ").map(Ingredient::from_str).collect())
    }
}

type Cookbook<'a> = Vec<Recipe<'a>>;

type Inventory<'a> = HashMap<&'a str, usize>;

fn transmute<'a>(inventory: &mut Inventory<'a>, recipe: &Recipe<'a>) {
    let Ingredient(amount, name) = recipe.0;
    let quantity = (inventory[name] + amount - 1) / amount;
    for Ingredient(amount, name) in &recipe.1 {
        *inventory.entry(*name).or_insert(0) += quantity * *amount;
    }
}

fn ore_required_for_fuel(fuel: usize, cookbook: &Cookbook) -> usize {
    let mut inventory = HashMap::from([("FUEL", fuel)]);
    for recipe in cookbook {
        transmute(&mut inventory, recipe);
    }
    inventory["ORE"]
}

fn parse_input() -> Cookbook<'static> {
    let mut recipes: HashMap<_, _> = include_str!("../../../../inputs/2019/14.txt").lines()
        .map(Recipe::from_str)
        .map(|recipe| (recipe.0.1, recipe))
        .collect();
    let dependencies = recipes.iter().map(|(&k, v)|
        (k, v.1.iter().map(|ing| ing.1).collect())
    ).collect();
    let ordering = lexmin_toposort(&dependencies);
    ordering.into_iter().filter_map(|&ingredient| recipes.remove(ingredient)).collect()
}

fn part1(cookbook: &Cookbook) -> usize {
    ore_required_for_fuel(1, cookbook)
}

fn part2(cookbook: &Cookbook) -> usize {
    let steps = iterate(1, |x| 2 * x).take_while(|&fuel| ore_required_for_fuel(fuel, cookbook) <= 1_000_000_000_000);
    steps.collect_vec().into_iter().rev().fold(0, |acc, x| if ore_required_for_fuel(acc + x, cookbook) <= 1_000_000_000_000 { acc + x } else { acc })
}

fn main() {
    let start_time = Instant::now();
    let cookbook = parse_input();
    let part1_ans = part1(&cookbook);
    let part2_ans = part2(&cookbook);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
