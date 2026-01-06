use itertools::Itertools;
use std::cmp::max;
use std::time::Instant;

type Tuple3 = (isize, isize, isize);

struct Cube {
    corner: Tuple3,
    length: isize,
}

struct Nanobot {
    pos: Tuple3,
    r: usize,
}

impl Nanobot {
    fn from_str(s: &str) -> Self {
        let mut numbers = s.split(|c: char| c != '-' && !c.is_numeric()).filter_map(|s| s.parse().ok());
        Nanobot { pos: numbers.next_tuple().unwrap(), r: numbers.next().unwrap() as usize }
    }
}

fn modulus(point: Tuple3) -> usize {
    point.0.unsigned_abs() + point.1.unsigned_abs() + point.2.unsigned_abs()
}

fn manhattan(pos1: Tuple3, pos2: Tuple3) -> usize {
    pos1.0.abs_diff(pos2.0) + pos1.1.abs_diff(pos2.1) + pos1.2.abs_diff(pos2.2)
}

fn bounding_box(bots: &[Nanobot]) -> (Tuple3, Tuple3) {
    let (lx, ux) = bots.iter().map(|bot| bot.pos.0).minmax().into_option().unwrap();
    let (ly, uy) = bots.iter().map(|bot| bot.pos.1).minmax().into_option().unwrap();
    let (lz, uz) = bots.iter().map(|bot| bot.pos.2).minmax().into_option().unwrap();
    ((lx, ly, lz), (ux, uy, uz))
}

fn distance_to_interval(lo: isize, hi: isize, x: isize) -> usize {
    if x < lo { lo.abs_diff(x) } else if x >= hi { hi.abs_diff(x) } else { 0 }
}

fn is_in_range_of(cube: &Cube, bot: &Nanobot) -> bool {
    let dx = distance_to_interval(cube.corner.0, cube.corner.0 + cube.length - 1, bot.pos.0);
    let dy = distance_to_interval(cube.corner.1, cube.corner.1 + cube.length - 1, bot.pos.1);
    let dz = distance_to_interval(cube.corner.2, cube.corner.2 + cube.length - 1, bot.pos.2);
    dx + dy + dz <= bot.r
}

fn bots_in_range_of(cube: &Cube, bots: &[Nanobot]) -> usize {
    bots.iter().filter(|&bot| is_in_range_of(cube, bot)).count()
}

fn lower_bound_for_bots_in_range_of_single_point(bots: &[Nanobot]) -> usize {
    let (lower, upper) = bounding_box(bots);
    let granularity = 1isize << (max(max(upper.0 - lower.0 + 1, upper.1 - lower.1 + 1), upper.2 - lower.2 + 1).ilog2() + 1);
    let (mut regions, mut best_hits) = (vec![Cube { corner: lower, length: granularity }], bots.len());

    while regions[0].length > 1 {
        let (mut next_regions, mut next_hits) = (Vec::new(), 0);
        for region in &regions {
            for x in [region.corner.0, region.corner.0 + region.length / 2] {
                for y in [region.corner.1, region.corner.1 + region.length / 2] {
                    for z in [region.corner.2, region.corner.2 + region.length / 2] {
                        let subregion = Cube { corner: (x, y, z), length: region.length / 2 };
                        let hits = bots_in_range_of(&subregion, bots);
                        if hits > next_hits { next_regions.clear(); next_hits = hits; }
                        if hits == next_hits { next_regions.push(subregion); }
                    }
                }
            }
        }
        (regions, best_hits) = (next_regions, next_hits);
    }

    best_hits
}

fn point_in_range_of_most_bots(bots: &[Nanobot]) -> Tuple3 {
    let hits_lb = lower_bound_for_bots_in_range_of_single_point(bots);
    let (lower, upper) = bounding_box(bots);
    let granularity = 1isize << (max(max(upper.0 - lower.0 + 1, upper.1 - lower.1 + 1), upper.2 - lower.2 + 1).ilog2() + 1);
    let mut regions = vec![(bots.len(), Cube { corner: lower, length: granularity })];

    while regions[0].1.length > 1 {
        let mut next_regions = Vec::new();
        for (_, region) in &regions {
            for x in [region.corner.0, region.corner.0 + region.length / 2] {
                for y in [region.corner.1, region.corner.1 + region.length / 2] {
                    for z in [region.corner.2, region.corner.2 + region.length / 2] {
                        let subregion = Cube { corner: (x, y, z), length: region.length / 2 };
                        let hits = bots_in_range_of(&subregion, bots);
                        if hits >= hits_lb { next_regions.push((hits, subregion)); }
                    }
                }
            }
        }
        regions = next_regions;
    }

    let best_hits = regions.iter().map(|(hits, _)| *hits).max().unwrap();
    let best_points = regions.iter().filter_map(|&(hits, ref cube)| (hits == best_hits).then_some(cube.corner));
    best_points.min_by_key(|&point| modulus(point)).unwrap()
}

fn parse_input() -> Vec<Nanobot> {
    include_str!("../../../../inputs/2018/23.txt").lines().map(Nanobot::from_str).collect()
}

fn part1(bots: &[Nanobot]) -> usize {
    let strongest_bot = bots.iter().max_by_key(|bot| bot.r).unwrap();
    bots.iter().filter(|bot| manhattan(bot.pos, strongest_bot.pos) <= strongest_bot.r).count()
}

fn part2(bots: &[Nanobot]) -> usize {
    modulus(point_in_range_of_most_bots(bots))
}

fn main() {
    let start_time = Instant::now();
    let bots = parse_input();
    let part1_ans = part1(&bots);
    let part2_ans = part2(&bots);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
