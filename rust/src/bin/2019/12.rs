use advent_of_code::math::lcm;
use itertools::Itertools;
use std::cmp::Ordering;
use std::time::Instant;

struct Moon {
    position: Vec<isize>,
    velocity: Vec<isize>,
}

impl Moon {
    fn at(position: Vec<isize>) -> Self {
        Self { velocity: vec![0; position.len()], position }
    }

    fn move_once(&mut self) {
        self.position.iter_mut().zip(self.velocity.iter()).for_each(|(pos, &vel)| { *pos += vel; })
    }

    fn energy(&self) -> usize {
        self.position.iter().map(|x| x.unsigned_abs()).sum::<usize>() * self.velocity.iter().map(|x| x.unsigned_abs()).sum::<usize>()
    }
}

fn step(moons: &mut [Moon]) {
    for (idx, jdx) in (0 .. moons.len()).tuple_combinations() {
        for kdx in 0 .. moons[idx].position.len() {
            match moons[idx].position[kdx].cmp(&moons[jdx].position[kdx]) {
                Ordering::Less => { moons[idx].velocity[kdx] += 1; moons[jdx].velocity[kdx] -=1 ; },
                Ordering::Greater => { moons[idx].velocity[kdx] -= 1; moons[jdx].velocity[kdx] +=1 ; },
                _ => {},
            }
        }
    }
    for moon in moons.iter_mut() { moon.move_once(); }
}

fn period(mut moons: Vec<Moon>) -> usize {
    let starting_position = moons.iter().map(|moon| (moon.position.clone(), moon.velocity.clone())).collect_vec();
    for steps in 1 .. {
        step(&mut moons);
        if moons.iter().zip(starting_position.iter()).all(|(moon, (pos, vel))| &moon.position == pos && &moon.velocity == vel) { return steps; }
    }
    unreachable!()
}

fn parse_input() -> Vec<Moon> {
    include_str!("../../../../inputs/2019/12.txt").lines()
        .map(|line| line.split(|c: char| c != '-' && !c.is_numeric()).filter_map(|s| s.parse().ok()).collect())
        .map(Moon::at)
        .collect()
}

fn part1(mut moons: Vec<Moon>) -> usize {
    for _ in 0 .. 1000 { step(&mut moons); }
    moons.iter().map(|moon| moon.energy()).sum()
}

fn part2(moons: &[Moon]) -> usize {
    let flattened_moons = (0 .. moons[0].position.len()).map(|kdx|
        moons.iter().map(|moon| Moon::at(vec![moon.position[kdx]])).collect_vec()
    );
    flattened_moons.map(period).reduce(lcm).unwrap()
}

fn main() {
    let start_time = Instant::now();
    let moons = parse_input();
    let part2_ans = part2(&moons);
    let part1_ans = part1(moons);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
