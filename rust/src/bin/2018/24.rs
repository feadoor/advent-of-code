use itertools::Itertools;
use regex::Regex;
use std::cmp::{min, Reverse};
use std::collections::HashMap;
use std::sync::LazyLock;
use std::time::Instant;

#[derive(Clone)]
struct Group {
    units: usize,
    hp: usize,
    initiative: usize,
    power: usize,
    attack_type: String,
    weaknesses: Vec<String>,
    immunities: Vec<String>,
}

impl Group {
    fn from_str(s: &str) -> Self {
        static GROUP_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
            r"(\d+) units each with (\d+) hit points (?:\((weak|immune) to ([^;\)]*)(?:; (weak|immune) to ([^\)]*))?\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)"
        ).unwrap());

        let captures = GROUP_RE.captures(s).unwrap();
        let units = captures[1].parse().unwrap();
        let hp = captures[2].parse().unwrap();

        let weaknesses = 
        if captures.get(3).is_some_and(|cap| cap.as_str() == "weak") { captures[4].split(", ").map(|s| s.to_owned()).collect() }
        else if captures.get(5).is_some_and(|cap| cap.as_str() == "weak") { captures[6].split(", ").map(|s| s.to_owned()).collect() }
        else { Vec::new() };

        let immunities = 
        if captures.get(3).is_some_and(|cap| cap.as_str() == "immune") { captures[4].split(", ").map(|s| s.to_owned()).collect() }
        else if captures.get(5).is_some_and(|cap| cap.as_str() == "immune") { captures[6].split(", ").map(|s| s.to_owned()).collect() }
        else { Vec::new() };

        let power = captures[7].parse().unwrap();
        let attack_type = captures[8].to_owned();
        let initiative = captures[9].parse().unwrap();

        Self { units, hp, initiative, power, attack_type, weaknesses, immunities }
    }
}

fn damage_key(attacker: &Group, defender: &Group) -> (usize, usize, usize) {
    let mut damage = if defender.immunities.contains(&attacker.attack_type) { 0 } else { attacker.units * attacker.power };
    if defender.weaknesses.contains(&attacker.attack_type) { damage *= 2; }
    (damage, defender.units * defender.power, defender.initiative)
}

fn battle(immune: &mut Vec<Group>, infection: &mut Vec<Group>) {
    while !immune.is_empty() && !infection.is_empty() {

        immune.sort_by_key(|g| Reverse((g.units * g.power, g.initiative)));
        infection.sort_by_key(|g| Reverse((g.units * g.power, g.initiative)));

        let mut selections = HashMap::new();
        for idx in 0 .. immune.len() {
            if let Some(jdx) = (0 .. infection.len()).filter(|&jdx| !selections.values().contains(&(1, jdx))).max_by_key(|&jdx| damage_key(&immune[idx], &infection[jdx])) {
                if damage_key(&immune[idx], &infection[jdx]).0 > 0 { selections.insert((0, idx), (1, jdx)); }
            }
        }
        for idx in 0 .. infection.len() {
            if let Some(jdx) = (0 .. immune.len()).filter(|&jdx| !selections.values().contains(&(0, jdx))).max_by_key(|&jdx| damage_key(&infection[idx], &immune[jdx])) {
                if damage_key(&infection[idx], &immune[jdx]).0 > 0 { selections.insert((1, idx), (0, jdx)); }
            }
        }

        let attacking_groups = selections.iter().sorted_by_key(|&(&(team, idx), _)| if team == 0 { Reverse(immune[idx].initiative) } else { Reverse(infection[idx].initiative) });
        let mut killed_someone = false;
        for (&(attacker_team, attacker_idx), &(_defender_team, defender_idx)) in attacking_groups {
            let (attacker, defender) = if attacker_team == 0 { (&mut immune[attacker_idx], &mut infection[defender_idx]) } else { (&mut infection[attacker_idx], &mut immune[defender_idx]) };
            let damage = damage_key(attacker, defender).0;
            let killed_units = min(defender.units, damage / defender.hp);
            defender.units -= killed_units;
            if killed_units > 0 { killed_someone = true; }
        }

        if !killed_someone { return; }
        *immune = immune.into_iter().filter_map(|g| (g.units > 0).then(|| g.clone())).collect();
        *infection = infection.into_iter().filter_map(|g| (g.units > 0).then(|| g.clone())).collect();
    }
}

fn parse_input() -> (Vec<Group>, Vec<Group>) {
    let mut lines = include_str!("../../../../inputs/2018/24.txt").lines().skip(1).peekable();
    let immune = lines.peeking_take_while(|line| line.starts_with(|c: char| c.is_numeric())).map(Group::from_str).collect();
    lines.next(); lines.next();
    let infection = lines.peeking_take_while(|line| line.starts_with(|c: char| c.is_numeric())).map(Group::from_str).collect();
    (immune, infection)
}

fn part1(mut immune: Vec<Group>, mut infection: Vec<Group>) -> usize {
    battle(&mut immune, &mut infection);
    immune.iter().map(|g| g.units).sum::<usize>() + infection.iter().map(|g| g.units).sum::<usize>()
}

fn part2(immune: &Vec<Group>, infection: &Vec<Group>) -> usize {
    for boost in 1.. {
        let mut immune = immune.iter().map(|g| { let mut g = g.clone(); g.power += boost; g }).collect();
        let mut infection = infection.clone();
        battle(&mut immune, &mut infection);
        if infection.len() == 0 { return immune.iter().map(|g| g.units).sum() }
    }

    unreachable!()
}

fn main() {
    let start_time = Instant::now();
    let (immune, infection) = parse_input();
    let part2_ans = part2(&immune, &infection);
    let part1_ans = part1(immune, infection);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
