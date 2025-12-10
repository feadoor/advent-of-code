use itertools::Itertools;

fn reverse(values: &mut [u8], start: usize, len: usize) {
    if start + len < values.len() {
        values[start .. start + len].reverse()
    } else {
        for idx in start .. start + len / 2 {
            values.swap(idx % values.len(), (2 * start + len - 1 - idx) % values.len());
        }
    }
}

pub fn twist(values: &mut [u8], lengths: &[usize], reps: usize) {
    let skips_and_lengths = (0..).zip(lengths.iter().cycle().take(reps * lengths.len()).copied());
    for (start, len) in skips_and_lengths.scan(0, |idx, (skip, len)| {
        let ret = Some((*idx, len));
        *idx += skip + len; ret
    }) {
        reverse(values, start, len);
    }
}

pub fn compute_hash(bytes: &[usize]) -> Vec<u8> {
    let mut values = (0 ..= 255).collect_vec();
    let mut lengths = bytes.to_vec();
    lengths.extend_from_slice(&[17, 31, 73, 47, 23]);
    twist(&mut values, &lengths, 64);
    values.iter().chunks(16).into_iter().map(|chunk| chunk.fold(0, |a, b| a ^ b)).collect()
}
