use advent_of_code::math::gcd;
use itertools::Itertools;
use regex::Regex;
use std::iter::repeat_n;
use std::sync::LazyLock;
use std::time::Instant;
use std::usize;

struct Row {
    indicators: usize,
    bitfields: Vec<usize>,
    joltage_reqs: Vec<isize>,
}

impl Row {
    fn from_str(s: &str) -> Self {
        static ROW_RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(
            r"\[(?<target>[\.#]+)\] (?<bitfields>[\d\(\), ]+) \{(?<joltage>[\d,]+)\}"
        ).unwrap());

        let captures = ROW_RE.captures(s).unwrap();
        Self {
            indicators: captures["target"].chars().rev().fold(0, |acc, c| (acc << 1) | (if c == '#' { 1 } else { 0 })),
            bitfields: captures["bitfields"].split(" ").map(|s| s[1 .. s.len() - 1].split(",").map(|n| n.parse::<usize>().unwrap()).fold(0, |acc, x| acc | (1 << x))).collect(),
            joltage_reqs: captures["joltage"].split(",").map(|n| n.parse().unwrap()).collect(),
        }
    }
}

fn fewest_presses_for_indicators(row: &Row) -> usize {
    (1 .. row.bitfields.len()).find(|&k| 
        row.bitfields.iter().combinations(k).any(|buttons| buttons.iter().fold(0, |acc, &&x| acc ^ x) == row.indicators)
    ).unwrap()
}

fn fewest_presses_for_joltage(row: &Row) -> isize {

    // Set up a system of linear equations using a matrix and an auxiliary vector
    let (n_eqs, n_vars) = (row.joltage_reqs.len(), row.bitfields.len());
    let mut mat = vec![vec![0; n_vars + 1]; n_eqs];
    for eq_idx in 0 .. n_eqs {
        for (var_idx, &bitfield) in row.bitfields.iter().enumerate() {
            if (bitfield & (1 << eq_idx)) != 0 {
                mat[eq_idx][var_idx] = 1;
            }
        }
        mat[eq_idx][n_vars] = row.joltage_reqs[eq_idx];
    }

    // Perform Gaussian elimination as far as possible on the system
    let (mut bound_vars, mut free_vars) = (vec![], vec![]);
    for var_idx in 0 .. n_vars {
        let row_idx = bound_vars.len();
        if let Some(pivot_idx) = (row_idx .. n_eqs).find(|&idx| mat[idx][var_idx] != 0) {
            mat.swap(row_idx, pivot_idx);
            for other_idx in 0 .. n_eqs { 
                if other_idx != row_idx && mat[other_idx][var_idx] != 0 {
                    let g = gcd(mat[other_idx][var_idx], mat[row_idx][var_idx]);
                    let (lambda1, lambda2) = (mat[other_idx][var_idx] / g, mat[row_idx][var_idx] / g);
                    for col in 0 ..= n_vars {
                        mat[row_idx][col] *= lambda1;
                        mat[other_idx][col] = lambda2 * mat[other_idx][col] - mat[row_idx][col];
                    }
                }
            }
            bound_vars.push(var_idx);
        } else {
            free_vars.push(var_idx);
        }
    }

    // Find the maximum value each free variable can take
    let maximum_values = free_vars.iter().map(|&var_idx| 
        (0 .. n_eqs)
            .filter(|&eq_idx| (row.bitfields[var_idx] & (1 << eq_idx)) != 0)
            .map(|eq_idx| row.joltage_reqs[eq_idx])
            .min().unwrap()
    ).collect_vec();

    // For each assignment to the free variables, find the full solution arising from this
    // assignment and check if its sum is smaller than the best known sum
    let mut best_sum = isize::MAX;
    let (mut free_sum, mut free_vals) = (0, vec![0; free_vars.len()]);
    loop {

        // Compute the bound variables and check that the sum of the solution doesn't
        // exceed the minimum sum found so far for any solution
        let mut total_sum = free_sum;
        if bound_vars.iter().enumerate().all(|(row_idx, &var_idx)| {
            let coeff = mat[row_idx][var_idx];
            let rhs = mat[row_idx][n_vars] - free_vars.iter().enumerate().map(|(i, &v)| mat[row_idx][v] * free_vals[i]).sum::<isize>();
            if rhs.signum() == -coeff.signum() || rhs % coeff != 0 {
                false
            } else {
                total_sum += rhs / coeff;
                total_sum < best_sum
            }
        }) {
            best_sum = total_sum;
        }

        // Move on to the next assignment of free variables, bearing in mind the best
        // bounds currently known both on each variable individually and on their sum
        while !free_vals.is_empty() && (free_sum >= best_sum || free_vals[free_vals.len() - 1] == maximum_values[free_vals.len() - 1]) {
            free_sum -= free_vals.pop().unwrap();
        }
        if let Some(val) = free_vals.last_mut() {
            *val += 1; free_sum += 1;
            free_vals.extend(repeat_n(0, free_vars.len() - free_vals.len()));
        } else {
            break;
        }
    };

    best_sum
}

fn parse_input() -> Vec<Row> {
    include_str!("../../../../inputs/2025/10.txt").lines().map(Row::from_str).collect()
}

fn part1(rows: &[Row]) -> usize {
    rows.iter().map(fewest_presses_for_indicators).sum()
}

fn part2(rows: &[Row]) -> isize {
    rows.iter().map(fewest_presses_for_joltage).sum()
}

fn main() {
    let start_time = Instant::now();
    let rows = parse_input();
    let part1_ans = part1(&rows);
    let part2_ans = part2(&rows);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
