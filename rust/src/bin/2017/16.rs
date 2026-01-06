use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

#[derive(Copy, Clone)]
enum Move {
    Spin(usize),
    Exchange(usize, usize),
    Partner(char, char),
}

impl Move {
    fn from_str(s: &str) -> Self {
        let mut chars = s.chars();
        let move_type = chars.next().unwrap();
        let rest: String = chars.collect();

        match move_type {
            's' => Self::Spin(rest.parse().unwrap()),
            'x' => {
                let mut nums = rest.split("/").map(|x| x.parse().unwrap());
                Self::Exchange(nums.next().unwrap(), nums.next().unwrap())
            },
            'p' => {
                let mut names = rest.split("/").map(|x| x.chars().next().unwrap());
                Self::Partner(names.next().unwrap(), names.next().unwrap())
            },
            x => panic!("Unrecognised move type {}", x),
        }
    }
}

fn make_move(programs: &mut [char], mov: Move) {
    match mov {
        Move::Spin(size) => programs.rotate_right(size),
        Move::Exchange(a, b) => programs.swap(a, b),
        Move::Partner(a, b) => {
            let (ix, jx) = (programs.iter().position(|&x| x == a).unwrap(), programs.iter().position(|&x| x == b).unwrap());
            programs.swap(ix, jx);
        }
    }
}

fn parse_input() -> Vec<Move> {
    include_str!("../../../../inputs/2017/16.txt").trim().split(",").map(Move::from_str).collect()
}

fn part1(moves: &[Move]) -> String {
    let mut programs = ('a' ..= 'p').collect_vec();
    for &mov in moves {
        make_move(&mut programs, mov);
    }
    programs.into_iter().join("")
}

fn part2(moves: &[Move], iterations: usize) -> String {
    let mut programs = ('a' ..= 'p').collect_vec();
    let mut seen: Vec<Vec<char>> = Vec::new();
    let mut lookup = HashMap::new();
    
    for it in 0 .. iterations {
        match lookup.get(&programs) {
            Some(idx) => return seen[idx + iterations % (it - idx)].iter().join(""),
            None => {
                seen.push(programs.clone());
                lookup.insert(programs.clone(), it);
                for &mov in moves {
                    make_move(&mut programs, mov)
                }
            }
        }
    }

    programs.iter().join("")
}

fn main() {
    let start_time = Instant::now();
    let moves = parse_input();
    let part1_ans = part1(&moves);
    let part2_ans = part2(&moves, 1_000_000_000);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
