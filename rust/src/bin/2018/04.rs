use itertools::Itertools;
use std::collections::HashMap;
use std::time::Instant;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Timestamp {
    year: usize,
    month: usize,
    day: usize,
    hour: usize,
    minute: usize,
}

impl Timestamp {
    fn from_str(s: &str) -> Self {
        let (year, month, day, hour, minute) = s.split(|c: char| !c.is_numeric()).filter_map(|s| s.parse().ok()).collect_tuple().unwrap();
        Self { year, month, day, hour, minute }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum EntryType {
    StartShift(usize),
    Asleep,
    Awake,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct Entry {
    time: Timestamp,
    typ: EntryType,
}

impl Entry {
    fn from_str(s: &str) -> Self {
        let (timestamp_str, rest) = s.split_once("] ").unwrap();
        let typ = if rest.ends_with("begins shift") {
            EntryType::StartShift(rest.split(|c: char| !c.is_numeric()).filter_map(|s| s.parse().ok()).exactly_one().unwrap())
        } else if rest.ends_with("falls asleep") {
            EntryType::Asleep
        } else {
            EntryType::Awake
        };

        Self {
            time: Timestamp::from_str(timestamp_str),
            typ,
        }
    }
}

fn parse_input() -> HashMap<(usize, usize), usize> {
    let entries = include_str!("../../../../inputs/2018/04.txt").lines().map(Entry::from_str);
    let (mut guard_id, mut sleep_start) = (0, 0);
    let mut result = HashMap::new();
    for entry in entries.sorted() {
        match entry.typ {
            EntryType::StartShift(id) => guard_id = id,
            EntryType::Asleep => sleep_start = entry.time.minute,
            EntryType::Awake => (sleep_start .. entry.time.minute).for_each(|minute| *result.entry((guard_id, minute)).or_insert(0) += 1),
        }
    }
    result
}

fn part1(sleep_data: &HashMap<(usize, usize), usize>) -> usize {
    let data_by_guard = sleep_data.iter()
        .map(|(&(guard, minute), &count)| (guard, (minute, count)))
        .into_grouping_map()
        .collect::<HashMap<_, _>>();
    
    let best_guard = data_by_guard.iter().max_by_key(|&(_, counts)| counts.values().sum::<usize>()).unwrap().0;
    let best_minute = data_by_guard[best_guard].iter().max_by_key(|&(_, &count)| count).unwrap().0;
    best_guard * best_minute
}

fn part2(sleep_data: &HashMap<(usize, usize), usize>) -> usize {
    let (best_guard, best_minute) = sleep_data.iter().max_by_key(|&(_, &count)| count).unwrap().0;
    best_guard * best_minute
}

fn main() {
    let start_time = Instant::now();
    let sleep_data = parse_input();
    let part1_ans = part1(&sleep_data);
    let part2_ans = part2(&sleep_data);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
