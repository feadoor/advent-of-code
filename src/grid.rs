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
