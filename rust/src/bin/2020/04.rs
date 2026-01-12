use std::collections::HashMap;
use std::time::Instant;

struct Passport<'a>(HashMap<&'a str, &'a str>);

impl<'a> Passport<'a> {
    fn from_str(s: &'a str) -> Self {
        Self(s.split_ascii_whitespace().filter_map(|field| field.split_once(":")).collect())
    }

    fn has_required_fields(&self) -> bool {
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].into_iter().all(|s| self.0.contains_key(s))
    }

    fn is_valid(&self) -> bool {
        self.is_valid_byr() && self.is_valid_iyr() && self.is_valid_eyr() && self.is_valid_hgt() && self.is_valid_hcl() && self.is_valid_ecl() && self.is_valid_pid()
    }

    fn is_valid_byr(&self) -> bool {
        self.is_valid_year("byr", 1920, 2002)
    }

    fn is_valid_iyr(&self) -> bool {
        self.is_valid_year("iyr", 2010, 2020)
    }
    
    fn is_valid_eyr(&self) -> bool {
        self.is_valid_year("eyr", 2020, 2030)
    }

    fn is_valid_hgt(&self) -> bool {
        self.0.get("hgt").is_some_and(|hgt| {
            let (hgt, unit) = hgt.split_at(hgt.len().saturating_sub(2));
            hgt.parse::<usize>().is_ok_and(|hgt|
                (unit == "cm" && 150 <= hgt && hgt <= 193) || (unit == "in" && 59 <= hgt && hgt <= 76)
            )
        })
    }

    fn is_valid_hcl(&self) -> bool {
        self.0.get("hcl").is_some_and(|hcl| {
            let (hash, hcl) = hcl.split_at(1);
            hash == "#" && hcl.len() == 6 && hcl.chars().all(|c| c.is_digit(16) && !c.is_ascii_uppercase())
        })
    }

    fn is_valid_ecl(&self) -> bool {
        self.0.get("ecl").is_some_and(|ecl|
            ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].contains(ecl)
        )
    }

    fn is_valid_pid(&self) -> bool {
        self.0.get("pid").is_some_and(|pid|
            pid.len() == 9 && pid.chars().all(|c| c.is_ascii_digit())
        )
    }

    fn is_valid_year(&self, key: &str, lo: usize, hi: usize) -> bool {
        self.0.get(key).is_some_and(|it| it.parse().is_ok_and(|it| lo <= it && it <= hi))
    }
}

fn parse_input() -> Vec<Passport<'static>> {
    include_str!("../../../../inputs/2020/04.txt")
        .split("\n\n").flat_map(|s| s.split("\n\r\n"))
        .map(|s| Passport::from_str(s)).collect()
}

fn part1(passports: &[Passport]) -> usize {
    passports.iter().filter(|pp| pp.has_required_fields()).count()
}

fn part2(passports: &[Passport]) -> usize {
    passports.iter().filter(|pp| pp.is_valid()).count()
}

fn main() {
    let start_time = Instant::now();
    let passports = parse_input();
    let part1_ans = part1(&passports);
    let part2_ans = part2(&passports);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
