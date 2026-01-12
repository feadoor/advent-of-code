use advent_of_code::grid::{Direction, make_move};
use itertools::Itertools;
use std::collections::BTreeMap;
use std::time::Instant;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
enum TurnBehaviour {
    TurnLeft,
    GoStraight,
    TurnRight,
}

impl TurnBehaviour {
    fn next(&self) -> Self {
        match self {
            TurnBehaviour::TurnLeft => TurnBehaviour::GoStraight,
            TurnBehaviour::GoStraight => TurnBehaviour::TurnRight,
            TurnBehaviour::TurnRight => TurnBehaviour::TurnLeft,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct Cart {
    direction: Direction,
    turn_behaviour: TurnBehaviour,
}

impl Cart {
    fn new(direction: Direction, turn_behaviour: TurnBehaviour) -> Self {
        Self { direction, turn_behaviour }
    }
}

impl Cart {
    fn try_from_char(c: char) -> Option<Self> {
        match c {
            '^' => Some(Self { direction: Direction::Up, turn_behaviour: TurnBehaviour::TurnLeft }),
            '<' => Some(Self { direction: Direction::Left, turn_behaviour: TurnBehaviour::TurnLeft }),
            'v' => Some(Self { direction: Direction::Down, turn_behaviour: TurnBehaviour::TurnLeft }),
            '>' => Some(Self { direction: Direction::Right, turn_behaviour: TurnBehaviour::TurnLeft }),
            _ => None,
        }
    }
}

fn turn(direction: Direction, turn_behaviour: TurnBehaviour) -> Direction {
    match turn_behaviour {
        TurnBehaviour::TurnLeft => direction.turn_left(),
        TurnBehaviour::TurnRight => direction.turn_right(),
        TurnBehaviour::GoStraight => direction,
    }
}

fn next_cart(c: char, cart: Cart) -> Cart {
    match c {
        '/' => match cart.direction {
            Direction::Up => Cart::new(Direction::Right, cart.turn_behaviour),
            Direction::Down => Cart::new(Direction::Left, cart.turn_behaviour),
            Direction::Left => Cart::new(Direction::Down, cart.turn_behaviour),
            Direction::Right => Cart::new(Direction::Up, cart.turn_behaviour),
        },
        '\\' => match cart.direction {
            Direction::Up => Cart::new(Direction::Left, cart.turn_behaviour),
            Direction::Down => Cart::new(Direction::Right, cart.turn_behaviour),
            Direction::Left => Cart::new(Direction::Up, cart.turn_behaviour),
            Direction::Right => Cart::new(Direction::Down, cart.turn_behaviour),
        },
        '+' => Cart::new(turn(cart.direction, cart.turn_behaviour), cart.turn_behaviour.next()),
        _ => cart,
    }
}

fn tick(map: &Vec<Vec<char>>, carts: &mut BTreeMap<(usize, usize), Cart>) -> Option<(usize, usize)> {
    let current_carts = carts.keys().copied().collect_vec();
    current_carts.into_iter().map(|(row, col)| {
        carts.remove(&(row, col)).and_then(|cart| {
            let (next_row, next_col) = make_move((row, col), cart.direction);
            if carts.remove(&(next_row, next_col)).is_some() {
                Some((next_col, next_row))
            } else {
                carts.insert((next_row, next_col), next_cart(map[next_row][next_col], cart));
                None
            }
        })
    }).fold(None, |acc, x| acc.or(x))
}

fn parse_input() -> (Vec<Vec<char>>, BTreeMap<(usize, usize), Cart>) {
    let map: Vec<Vec<_>> = include_str!("../../../../inputs/2018/13.txt").lines().map(|line| line.chars().collect()).collect();
    let carts = map.iter().enumerate().flat_map(|(row, line)| 
        line.iter().enumerate().filter_map(move |(col, c)| 
            Cart::try_from_char(*c).map(|cart| ((row, col), cart))
        )
    ).collect();
    (map, carts)
}

fn part1(map: &Vec<Vec<char>>, carts: &mut BTreeMap<(usize, usize), Cart>) -> (usize, usize) {
    (0..).filter_map(|_| tick(map, carts)).next().unwrap()
}

fn part2(map: &Vec<Vec<char>>, carts: &mut BTreeMap<(usize, usize), Cart>) -> (usize, usize) {
    while carts.len() > 1 { tick(map, carts); }
    carts.keys().next().map(|&(r, c)| (c, r)).unwrap()
}

fn main() {
    let start_time = Instant::now();
    let (map, mut carts) = parse_input();
    let part1_ans = part1(&map, &mut carts);
    let part2_ans = part2(&map, &mut carts);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {:?}", part1_ans);
    println!("Part 2: {:?}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
