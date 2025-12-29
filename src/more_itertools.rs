use std::collections::VecDeque;

pub struct InterleaveAll<I> {
    iters: VecDeque<I>
}

impl <I> InterleaveAll<I> {
    fn new<J: IntoIterator<Item = I>>(iter: J) -> Self {
        Self { iters: iter.into_iter().collect() }
    }
}

impl<I: Iterator> Iterator for InterleaveAll<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(mut iter) = self.iters.pop_front() {
            if let Some(item) = iter.next() {
                self.iters.push_back(iter);
                return Some(item);
            }
        }
        None
    }
}

pub trait InterleaveAllExt : Iterator + Sized {
    fn interleave_all(self) -> InterleaveAll<Self::Item> {
        InterleaveAll::new(self)
    }
}

impl<I: Iterator> InterleaveAllExt for I {}
