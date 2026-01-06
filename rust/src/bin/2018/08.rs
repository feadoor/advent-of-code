use std::time::Instant;

struct Node {
    children: Vec<Node>,
    metadata: Vec<usize>,
}

impl Node {
    fn from_stream<I>(stream: &mut I) -> Self where I: Iterator<Item = usize> {
        let (n_children, n_meta) = (stream.next().unwrap(), stream.next().unwrap());
        let children = (0 .. n_children).map(|_| Node::from_stream(stream)).collect();
        let metadata = stream.take(n_meta).collect();
        Self { children, metadata }
    }

    fn sum_of_metadata(&self) -> usize {
        self.metadata.iter().sum::<usize>() + self.children.iter().map(|child| child.sum_of_metadata()).sum::<usize>()
    }

    fn value(&self) -> usize {
        match self.children.len() {
            0 => self.metadata.iter().sum(),
            _ => self.metadata.iter().filter_map(|&idx| self.children.get(idx - 1).map(|child| child.value())).sum(),
        }
    }
}

fn parse_input() -> Node {
    Node::from_stream(&mut include_str!("../../../../inputs/2018/08.txt").trim().split(" ").map(|s| s.parse().unwrap()))
}

fn part1(root: &Node) -> usize {
    root.sum_of_metadata()
}

fn part2(root: &Node) -> usize {
    root.value()
}

fn main() {
    let start_time = Instant::now();
    let root = parse_input();
    let part1_ans = part1(&root);
    let part2_ans = part2(&root);
    let elapsed_time = start_time.elapsed();

    println!("Part 1: {}", part1_ans);
    println!("Part 2: {}", part2_ans);
    println!("Elapsed: {:?}", elapsed_time);
}
