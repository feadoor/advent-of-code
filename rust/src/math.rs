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

pub fn mod_inv<T: PrimInt>(x: T, m: T) -> Option<T> {
    let (mut u1, mut u3) = (T::one(), x);
    let (mut v1, mut v3) = (T::zero(), m);
    let mut odd_iterations = false;

    while !v3.is_zero() {
        let q = u3 / v3;
        u1 = u1 + q * v1;
        u3 = u3 - q * v3;
        swap(&mut u1, &mut v1);
        swap(&mut u3, &mut v3);
        odd_iterations = !odd_iterations;
    }

    if u3.is_one() { if odd_iterations { Some(m - u1) } else { Some(u1) } } 
    else { None }
}

pub fn chinese_remainder<T: PrimInt>((a, r): (T, T), (b, s): (T, T)) -> Option<(T, T)> {
    mod_inv(s, r).and_then(|s_inv| mod_inv(r, s).map(|r_inv| {
        (mod_add(s * mod_mul(a, s_inv, r), r * mod_mul(b, r_inv, s), r * s), r * s)
    }))
}
