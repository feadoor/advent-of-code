use advent_of_code::linked_list::LinkedList;
use itertools::{Itertools, iterate};
use std::time::Instant;

fn parse_input() -> (usize, usize) {
    include_str!("../../../../inputs/2018/09.txt").split(" ").filter_map(|s| s.parse().ok()).collect_tuple().unwrap()
}

fn winning_score(players: usize, turns: usize) -> usize {
    let mut marbles = LinkedList::from([0]);
    let mut current_marble = marbles.head().unwrap();
    let mut scores = vec![0; players];

    for (turn, player) in (1 ..= turns).zip((0 .. players).cycle()) {
        if turn % 23 != 0 {
            let next_marble = marbles.next_circular(&current_marble);
            current_marble = marbles.insert_after(&next_marble, turn);
        }

        else {
            let extra_marble = iterate(current_marble, |x| marbles.prev_circular(x)).nth(7).unwrap();
            scores[player] += turn + marbles.val(&extra_marble);
            current_marble = marbles.next_circular(&extra_marble);
            marbles.remove(extra_marble);
        }
    }

    scores.into_iter().max().unwrap()
}

fn main() {
    let start_time = Instant::now();
    let (players, turns) = parse_input();
    let part1_ans = winning_score(players, turns);
    let part2_ans = winning_score(players, 100 * turns);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
