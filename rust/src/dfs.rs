pub enum Pruning {
    Above,
    Below,
    None,
}

pub trait DepthFirstTraversable {
    type Step: Sized;
    type Output;

    fn next_steps(&mut self) -> Vec<Self::Step>;
    fn apply_step(&mut self, step: &Self::Step);
    fn revert_step(&mut self, step: &Self::Step);
    fn should_prune(&mut self) -> Pruning;
    fn output(&mut self) -> Option<Self::Output>;
}

enum EdgeTraversal<S> {
    Apply(S),
    Revert(S),
    StartSearch,
    EndSearch,
}

pub struct DepthFirstSearcher<S, T> {
    state: T,
    steps: Vec<EdgeTraversal<S>>,
}

impl<S, T: DepthFirstTraversable<Step = S>> DepthFirstSearcher<S, T> {

    pub fn new(start: T) -> DepthFirstSearcher<S, T> {
        DepthFirstSearcher { state: start, steps: vec![EdgeTraversal::StartSearch] }
    }

    pub fn run_search(&mut self) {
        while let Some(_) = self.next() {}
    }

    pub fn current_state(&self) -> &T {
        &self.state
    }

    fn apply_step(&mut self, step: T::Step) {
        self.state.apply_step(&step);
        self.steps.push(EdgeTraversal::Revert(step));
    }

    fn add_child_steps(&mut self) {
        self.steps.extend(self.state.next_steps().into_iter().rev().map(|step| EdgeTraversal::Apply(step)));
    }
}

impl<S, T: DepthFirstTraversable<Step = S>> Iterator for DepthFirstSearcher<S, T> {
    type Item = T::Output;

    fn next(&mut self) -> Option<T::Output> {
        use self::EdgeTraversal::*;

        while let Some(step) = self.steps.pop() {
            match step {
                StartSearch => {
                    self.steps.push(EndSearch);
                    if let Pruning::None = self.state.should_prune() {
                        self.add_child_steps();
                    }
                },
                EndSearch => {
                    if let Some(value) = self.state.output() {
                        return Some(value);
                    }
                },
                Apply(step) => {
                    self.apply_step(step);
                    match self.state.should_prune() {
                        Pruning::Above => {
                            if let Some(Revert(step)) = self.steps.pop() {
                                self.state.revert_step(&step);
                            }
                        },
                        Pruning::Below => {},
                        Pruning::None => { self.add_child_steps(); },
                    }
                },
                Revert(step) => {
                    let output = self.state.output();
                    self.state.revert_step(&step);
                    if output.is_some() { return output; }
                }
            }
        }

        None
    }
}
