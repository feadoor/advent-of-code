use itertools::Itertools;
use regex::Regex;
use std::sync::LazyLock;
use std::time::Instant;

struct Tuple3(isize, isize, isize);

impl Tuple3 {
    fn from_str(s: &str) -> Self {
        let mut values = s.split(",").map(|s| s.parse().unwrap());
        Tuple3(values.next().unwrap(), values.next().unwrap(), values.next().unwrap())
    }

    fn squared_magnitude(&self) -> isize {
        self.0 * self.0 + self.1 * self.1 + self.2 * self.2
    }

    fn sub(&self, other: &Tuple3) -> Tuple3 {
        Tuple3(self.0 - other.0, self.1 - other.1, self.2 - other.2)
    }
}

struct Particle {
    position: Tuple3,
    velocity: Tuple3,
    acceleration: Tuple3,
}

impl Particle {
    fn from_str(s: &str) -> Self {
        static PARTICLE_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
            r"p=<(?<pos>.*)>, v=<(?<vel>.*)>, a=<(?<acc>.*)>"
        ).unwrap());

        let captures = PARTICLE_RE.captures(s).unwrap();
        Self {
            position: Tuple3::from_str(&captures["pos"]),
            velocity: Tuple3::from_str(&captures["vel"]),
            acceleration: Tuple3::from_str(&captures["acc"]),
        }
    }
}

fn integer_sqrt(v: isize) -> Option<isize> {
    if let Some(s) = v.checked_isqrt() {
        if s * s == v { Some(s) } else { None }
    } else {
        None
    }
}

fn quadratic_solutions(a: isize, b: isize, c: isize) -> Vec<isize> {
    if a == 0 && b == 0 { Vec::new() } 
    else if a == 0 { if c % b == 0 { vec![-c / b] } else { Vec::new() }}
    else {
        let discriminant = b * b - 4 * a * c;
        if let Some(s) = integer_sqrt(discriminant) {
            [-b + s, -b - s].into_iter().filter(|n| n % (2 * a) == 0).map(|n| n / (2 * a)).collect()
        } else {
            Vec::new()
        }
    }
}

fn collision_index(p1: &Particle, p2: &Particle) -> Option<isize> {
    let deltas = [p1.position.sub(&p2.position), p1.velocity.sub(&p2.velocity), p1.acceleration.sub(&p2.acceleration)];
    let equations = [
        (deltas[2].0, 2 * deltas[1].0 + deltas[2].0, 2 * deltas[0].0),
        (deltas[2].1, 2 * deltas[1].1 + deltas[2].1, 2 * deltas[0].1),
        (deltas[2].2, 2 * deltas[1].2 + deltas[2].2, 2 * deltas[0].2),
    ];
    let solutions = equations.into_iter()
        .filter(|&(a, b, c)| a != 0 || b != 0 || c != 0)
        .map(|(a, b, c)| quadratic_solutions(a, b, c))
        .collect_vec();
    
    solutions[0].iter().copied()
        .filter(|&x| x >= 0 && solutions.iter().skip(1).all(|sols| sols.contains(&x)))
        .min()
}

fn parse_input() -> Vec<Particle> {
    include_str!("../../../../inputs/2017/20.txt").lines()
        .map(Particle::from_str)
        .collect()
}

fn part1(particles: &[Particle]) -> usize {
    (0 .. particles.len()).min_by_key(|&idx| particles[idx].acceleration.squared_magnitude()).unwrap()
}

fn part2(particles: &[Particle]) -> usize {
    let mut collision_indices = vec![vec![None; particles.len()]; particles.len()];
    for idx in 0 .. particles.len() {
        for jdx in idx + 1 .. particles.len() {
            if let Some(collision) = collision_index(&particles[idx], &particles[jdx]) {
                collision_indices[idx][jdx] = Some(collision);
                collision_indices[jdx][idx] = Some(collision);
            }
        }
    }
    
    (0 .. particles.len()).filter(|&idx| {
        !collision_indices[idx].iter().enumerate().any(|(jdx, &coll)| {
            coll.is_some() && collision_indices[jdx].iter().filter_map(|x| *x).min().unwrap() == coll.unwrap()
        })
    }).count()
}

fn main() {
    let start_time = Instant::now();
    let particles = parse_input();
    let part1_ans = part1(&particles);
    let part2_ans = part2(&particles);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
