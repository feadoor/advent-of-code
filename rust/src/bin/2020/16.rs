use itertools::Itertools;
use std::collections::HashSet;
use std::time::Instant;
use advent_of_code::matching::find_unique_matching;

type Range = (usize, usize);
type Field = (String, Vec<Range>);

fn parse_field(s: &str) -> Field {
    let (name, ranges) = s.split_once(": ").unwrap();
    let ranges = ranges.split(" or ").map(|range| range.split('-').map(|val| val.parse().unwrap()).collect_tuple().unwrap()).collect();
    (name.to_string(), ranges)
}

fn invalid_for_fields(fields: &[Field], val: usize) -> bool {
    fields.iter().all(|field| field.1.iter().all(|&(lo, hi)| val < lo || val > hi))
}

fn possible_assignments<'a>(fields: &[Field], valid_tickets: impl Iterator<Item = &'a Vec<usize>>) -> Vec<HashSet<usize>> {
    let mut assignments = vec![(0 .. fields.len()).collect(); fields.len()];
    for ticket in valid_tickets {
        for (left_idx, &val) in ticket.iter().enumerate() {
            let matching_fields = (0 .. fields.len()).filter(|&right_idx| fields[right_idx].1.iter().any(|&(lo, hi)| lo <= val && val <= hi));
            assignments[left_idx] = &assignments[left_idx] & &matching_fields.collect();
        }
    }
    assignments
}

fn perfect_matching<'a>(fields: &[Field], valid_tickets: impl Iterator<Item = &'a Vec<usize>>) -> Vec<usize> {
    let assignments = possible_assignments(fields, valid_tickets);
    find_unique_matching(assignments).unwrap()
}

fn parse_input() -> (Vec<Field>, Vec<usize>, Vec<Vec<usize>>) {
    let mut lines = include_str!("../../../../inputs/2020/16.txt").lines();
    let fields = lines.by_ref().take_while(|line| !line.is_empty()).map(parse_field).collect();
    let your_ticket = lines.by_ref().skip(1).next().unwrap().split(',').map(|val| val.parse().unwrap()).collect();
    let nearby_tickets = lines.skip(2).map(|line| line.split(',').map(|val| val.parse().unwrap()).collect()).collect();
    (fields, your_ticket, nearby_tickets)
}

fn part1(fields: &[Field], nearby_tickets: &[Vec<usize>]) -> usize {
    nearby_tickets.iter().flatten().filter(|&&val| invalid_for_fields(fields, val)).sum()
}

fn part2(fields: &[Field], your_ticket: &[usize], nearby_tickets: &[Vec<usize>]) -> usize {
    let valid_tickets = nearby_tickets.iter().filter(|ticket| ticket.iter().all(|&val| !invalid_for_fields(fields, val)));
    let matching = perfect_matching(fields, valid_tickets);
    (0 .. fields.len()).filter(|&idx| fields[matching[idx]].0.starts_with("departure")).map(|idx| your_ticket[idx]).product()
}

fn main() {
    let start_time = Instant::now();
    let (fields, your_ticket, nearby_tickets) = parse_input();
    let part1_ans = part1(&fields, &nearby_tickets);
    let part2_ans = part2(&fields, &your_ticket, &nearby_tickets);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
