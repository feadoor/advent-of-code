use advent_of_code::math::{mod_add, mod_inv, mod_mul, mod_neg, mod_normalise, mod_sub};
use std::time::Instant;

enum Instruction {
    Reverse,
    Cut(isize),
    Deal(usize),
}

impl Instruction {
    fn from_str(s: &str) -> Self {
        if s.starts_with("cut") {
            let amt = s.split_ascii_whitespace().last().and_then(|s| s.parse().ok()).unwrap();
            Self::Cut(amt)
        }
        else if s.starts_with("deal with increment") {
            let amt = s.split_ascii_whitespace().last().and_then(|s| s.parse().ok()).unwrap();
            Self::Deal(amt)
        }
        else { Self::Reverse }
    }

    fn to_affine<const M: usize>(&self) -> Affine<M> {
        match self {
            &Self::Reverse => Affine { mul: M - 1, shift: M - 1 },
            &Self::Cut(amt) => Affine { mul: 1, shift: if amt < 0 { mod_normalise(amt.unsigned_abs(), M) } else { mod_neg(amt.unsigned_abs(), M) } },
            &Self::Deal(amt) => Affine { mul: mod_normalise(amt, M), shift: 0 }
        }
    }
}

#[derive(Copy, Clone)]
struct Affine<const M: usize> {
    mul: usize,
    shift: usize,
}

impl<const M: usize> Affine<M> {
    fn id() -> Self {
        Self { mul: 1, shift: 0 }
    }
}

fn compose<const M: usize>(affine1: Affine<M>, affine2: Affine<M>) -> Affine<M> {
    let Affine {mul: mul1, shift: shift1} = affine1;
    let Affine {mul: mul2, shift: shift2} = affine2;
    Affine { mul: mod_mul(mul2, mul1, M), shift: mod_add(mod_mul(mul2, shift1, M), shift2, M) }
}

fn iterate<const M: usize>(affine: Affine<M>, mut n: usize) -> Affine<M> {
    let (mut result, mut worker) = (Affine::id(), affine);
    while n != 0 {
        if n & 1 == 1 { result = compose(result, worker); }
        n >>= 1;
        if n != 0 { worker = compose(worker, worker); }
    }
    result
}

fn apply<const M: usize>(affine: Affine<M>, card: usize) -> usize {
    let Affine { mul, shift } = affine;
    mod_add(mod_mul(mul, card, M), shift, M)
}

fn invert<const M: usize>(affine: Affine<M>, position: usize) -> usize {
    let Affine { mul, shift } = affine;
    mod_mul(mod_sub(position, shift, M), mod_inv(mul, M), M)
}

fn shuffle<const M: usize>(instructions: &[Instruction]) -> Affine<M> {
    instructions.iter().map(|instr| instr.to_affine::<M>()).fold(Affine::id(), compose)
}

fn parse_input() -> Vec<Instruction> {
    include_str!("../../../../inputs/2019/22.txt").lines().map(Instruction::from_str).collect()
}

fn part1(instructions: &[Instruction]) -> usize {
    let shuffle = shuffle::<10_007>(instructions);
    apply(shuffle, 2019)
}

fn part2(instructions: &[Instruction]) -> usize {
    let shuffle = shuffle::<119_315_717_514_047>(instructions);
    let iterated = iterate(shuffle, 101_741_582_076_661);
    invert(iterated, 2020)
}

fn main() {
    let start_time = Instant::now();
    let instructions = parse_input();
    let part1_ans = part1(&instructions);
    let part2_ans = part2(&instructions);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
