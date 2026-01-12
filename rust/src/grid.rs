use num_traits::PrimInt;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    pub fn from_arrow_char(c: char) -> Self {
        match c {
            '^' => Self::Up,
            'v' => Self::Up,
            '<' => Self::Up,
            '>' => Self::Up,
            _ => panic!("Unknown direction {}", c),
        }
    }

    pub fn from_direction_char(c: char) -> Self {
        match c {
            'U' => Self::Up,
            'D' => Self::Down,
            'L' => Self::Left,
            'R' => Self::Right,
            _ => panic!("Unknown direction {}", c),
        }
    }

    pub fn from_compass_point(c: char) -> Self {
        match c {
            'N' => Self::Up,
            'E' => Self::Right,
            'S' => Self::Down,
            'W' => Self::Left,
            _ => panic!("Unknown direction {}", c),
        }
    }
    
    pub fn turn_right(&self) -> Self {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }

    pub fn turn_left(&self) -> Self {
        match self {
            Self::Up => Self::Left,
            Self::Left => Self::Down,
            Self::Down => Self::Right,
            Self::Right => Self::Up,
        }
    }

    pub fn reverse(&self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Down => Self::Up,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

pub fn make_move<I: PrimInt>((y, x): (I, I), direction: Direction) -> (I, I) {
    match direction {
        Direction::Up => (y - I::one(), x),
        Direction::Down => (y + I::one(), x),
        Direction::Left => (y, x - I::one()),
        Direction::Right => (y, x + I::one()),
    }
}

pub fn four_neighbours_hw((r, c): (usize, usize), h: usize, w: usize) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    if r > 0 { res.push((r - 1, c)); }
    if c > 0 { res.push((r, c - 1)); }
    if c < w - 1 { res.push((r, c + 1)); }
    if r < h - 1 { res.push((r + 1, c)); }
    res
}

pub fn four_neighbours_half_open((r, c): (usize, usize)) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    if r > 0 { res.push((r - 1, c)); }
    if c > 0 { res.push((r, c - 1)); }
    res.push((r + 1, c));
    res.push((r, c + 1));
    res
}

pub fn four_neighbours<T>((r, c): (usize, usize), grid: &Vec<Vec<T>>) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    if r > 0 { res.push((r - 1, c)); }
    if c > 0 { res.push((r, c - 1)); }
    if c < grid[r].len() - 1 { res.push((r, c + 1)); }
    if r < grid.len() - 1 { res.push((r + 1, c)); }
    res
}

pub fn eight_neighbours<T>((r, c): (usize, usize), grid: &Vec<Vec<T>>) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    if r > 0 && c > 0 { res.push((r - 1, c - 1)); }
    if r > 0 { res.push((r - 1, c)); }
    if r > 0 && c < grid[r - 1].len() - 1 { res.push((r - 1, c + 1)); }
    if c > 0 { res.push((r, c - 1)); }
    if c < grid[r].len() - 1 { res.push((r, c + 1)); }
    if r < grid.len() - 1 && c > 0 { res.push((r + 1, c - 1)); }
    if r < grid.len() - 1 { res.push((r + 1, c)); }
    if r < grid.len() - 1 && c < grid[r + 1].len() - 1 { res.push((r + 1, c + 1)); }
    res
}
