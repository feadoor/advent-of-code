use advent_of_code::grid::four_neighbours;
use itertools::{Itertools, iproduct};
use std::collections::{HashSet, VecDeque};
use std::time::Instant;

#[derive(Copy, Clone, PartialEq, Eq)]
enum Race {
    Elf,
    Goblin,
}

#[derive(Copy, Clone)]
struct Unit {
    race: Race,
    attack: usize,
    hp: usize,
}

struct Battlefield<'a> {
    grid: &'a Vec<Vec<char>>,
    units: Vec<Vec<Option<Unit>>>,
}

impl<'a> Battlefield<'a> {
    fn new(grid: &'a Vec<Vec<char>>, unit_hp: usize, elf_attack: usize, goblin_attack: usize) -> Self {
        Self { 
            grid,
            units: grid.iter().map(|row| row.iter().map(|c| match c {
                'E' => Some(Unit { race: Race::Elf, attack: elf_attack, hp: unit_hp }),
                'G' => Some(Unit { race: Race::Goblin, attack: goblin_attack, hp: unit_hp }),
                _ => None,
            }).collect()).collect()
        }
    }

    fn enemy_cells<I: IntoIterator<Item = (usize, usize)>>(&self, cells: I, unit: Unit) -> impl Iterator<Item = (usize, usize)> {
        cells.into_iter().filter(move |&(r, c)| self.units[r][c].filter(|it| it.race != unit.race).is_some())
    }

    fn contains_enemy<I: IntoIterator<Item = (usize, usize)>>(&self, cells: I, unit: Unit) -> bool {
        self.enemy_cells(cells, unit).next().is_some()
    }

    fn next_step(&self, (r, c): (usize, usize), unit: Unit) -> (usize, usize) {
        let (mut paths, mut visited) = (VecDeque::from([vec![(r, c)]]), HashSet::from([(r, c)]));
        let mut target_paths = Vec::new();

        while let Some(path) = paths.pop_front() {
            let (r, c) = *path.last().unwrap();
            if self.contains_enemy(four_neighbours((r, c), self.grid), unit) {
                target_paths.push(path);
            } else {
                for nbr in four_neighbours((r, c), self.grid).into_iter().filter(|&(b, a)| self.grid[b][a] != '#' && self.units[b][a].is_none()) {
                    if visited.insert(nbr) {
                        let mut new_path = path.clone(); new_path.push(nbr);
                        paths.push_back(new_path);
                    }
                }
            }
        }

        target_paths.into_iter().min_by_key(|path| (path.len(), *path.last().unwrap())).map(|path| *path.get(1).unwrap_or(&(r, c))).unwrap_or((r, c))
    }

    fn perform_round(&mut self) {
        let (height, width) = (self.grid.len(), self.grid[0].len());
        let move_order = iproduct!(0 .. height, 0 .. width).filter_map(|(r, c)| self.units[r][c].is_some().then_some((r, c))).collect_vec();
        for (r, c) in move_order {
            if let Some(unit) = self.units[r][c] {
                let (next_r, next_c) = self.next_step((r, c), unit);
                self.units[next_r][next_c] = self.units[r][c].take();
                if let Some((ty, tx)) = self.enemy_cells(four_neighbours((next_r, next_c), self.grid), unit).min_by_key(|&(b, a)| (self.units[b][a].unwrap().hp, b, a)) {
                    let mut enemy = self.units[ty][tx].take().unwrap();
                    if enemy.hp >= unit.attack { 
                        enemy.hp -= unit.attack;
                        self.units[ty][tx] = Some(enemy);
                    }
                }
            }
        }
    }

    fn count_elves(&self) -> usize {
        let (height, width) = (self.grid.len(), self.grid[0].len());
        iproduct!(0 .. height, 0 .. width).filter(|&(r, c)| self.units[r][c].is_some_and(|it| it.race == Race::Elf)).count()
    }

    fn total_hp_remaining(&self) -> usize {
        let (height, width) = (self.grid.len(), self.grid[0].len());
        iproduct!(0 .. height, 0 .. width).filter_map(|(r, c)| self.units[r][c]).map(|unit| unit.hp).sum()
    }

    fn combat_finished(&self) -> bool {
        let (height, width) = (self.grid.len(), self.grid[0].len());
        iproduct!(0 .. height, 0 .. width).filter_map(|(r, c)| self.units[r][c]).map(|unit| unit.race).all_equal()
    }
}

fn parse_input() -> Vec<Vec<char>> {
    include_str!("../../../../inputs/2018/15.txt").lines().map(|line| line.chars().collect()).collect()
}

fn part1(grid: &Vec<Vec<char>>) -> usize {
    let (mut battlefield, mut rounds) = (Battlefield::new(grid, 200, 3, 3), 0);
    while !battlefield.combat_finished() {
        battlefield.perform_round();
        rounds += 1;
    }
    battlefield.total_hp_remaining() * (rounds - 1)
}

fn part2(grid: &Vec<Vec<char>>) -> usize {
    (1..).filter_map(|elf_attack| {
        let (mut battlefield, mut rounds) = (Battlefield::new(grid, 200, elf_attack, 3), 0);
        let elves = battlefield.count_elves();
        while !battlefield.combat_finished() && battlefield.count_elves() == elves {
            battlefield.perform_round();
            rounds += 1;
        }
        (battlefield.combat_finished() && battlefield.count_elves() == elves).then(|| battlefield.total_hp_remaining() * (rounds - 1))
    }).next().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let grid = parse_input();
    let part1_ans = part1(&grid);
    let part2_ans = part2(&grid);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
