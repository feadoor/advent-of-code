use num_traits::PrimInt;

pub fn gcd<T: PrimInt>(mut x: T, mut y: T) -> T {
    while y != T::zero() {
        (x, y) = (y, x % y);
    }
    x
}
