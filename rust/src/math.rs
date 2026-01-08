use num_traits::PrimInt;
use std::cmp::Ordering;
use std::mem::swap;

pub fn gcd<T: PrimInt>(mut x: T, mut y: T) -> T {
    while y != T::zero() {
        (x, y) = (y, x % y);
    }
    x
}

pub fn lcm<T: PrimInt>(x: T, y: T) -> T {
    x * (y / gcd(x, y))
}

pub fn mod_normalise<T: PrimInt>(x: T, m: T) -> T {
    match x.cmp(&m) {
        Ordering::Greater => x % m,
        Ordering::Equal => T::zero(),
        Ordering::Less => match x.cmp(&T::zero()) {
            Ordering::Less => (x % m) + m,
            _ => x,
        }
    }
}

pub fn mod_neg<T: PrimInt>(x: T, m: T) -> T {
    let x = mod_normalise(x, m);
    if x == T::zero() { T::zero() } else { m - x }
}

pub fn mod_add<T: PrimInt>(x: T, y: T, m: T) -> T {
    let (x, y) = (mod_normalise(x, m), mod_normalise(y, m));
    match x + y {
        z if z >= m => z - m,
        z => z,
    }
}

pub fn mod_sub<T: PrimInt>(x: T, y: T, m: T) -> T {
    let (x, y) = (mod_normalise(x, m), mod_normalise(y, m));
    match x.cmp(&y) {
        Ordering::Less => x + m - y,
        Ordering::Equal => T::zero(),
        Ordering::Greater => x - y,
    }
}

pub fn mod_mul<T: PrimInt>(x: T, y: T, m: T) -> T {
    let (mut x, mut y) = (mod_normalise(x, m), mod_normalise(y, m));
    match x.checked_mul(&y) {
        Some(z) => mod_normalise(z, m),
        None => {
            if x > y { swap(&mut x, &mut y); }
            let mut result = T::zero();
            while !x.is_zero() {
                if x & T::one() == T::one() { result = mod_add(result, y, m); }
                x = x >> 1;
                y = mod_add(y, y, m);
            }
            result
        }
    }
}

pub fn mod_pow<T: PrimInt>(base: T, mut exp: T, m: T) -> T {
    let (mut answer, mut worker) = (T::one(), base);
    while !exp.is_zero() {
        if (exp & T::one()).is_one() { answer = mod_mul(answer, worker, m); }
        exp = exp >> 1;
        if !exp.is_zero() { worker = mod_mul(worker, worker, m); }
    }
    answer
}

pub fn mod_inv<T: PrimInt>(x: T, m: T) -> T {
    // Assumes that m is prime!
    mod_pow(x, m - T::one() - T::one(), m)
}
