use std::collections::HashSet;

pub struct UnionFind {
    nodes: Vec<UnionFindNode>,
    representatives: HashSet<usize>,
    size: usize,
}

impl UnionFind {
    pub fn new(size: usize) -> Self {
        Self { 
            nodes: (0..size).map(UnionFindNode::new_at_index).collect(),
            representatives: (0..size).collect(),
            size
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn size_of_set_containing(&mut self, a: usize) -> usize {
        self.find_representative(a).map(|rep| self.nodes[rep].size).unwrap_or(0)
    }

    pub fn all_set_sizes(&self) -> impl Iterator<Item = usize> {
        self.representatives.iter().map(|&idx| self.nodes[idx].size)
    }

    pub fn merge(&mut self, a: usize, b: usize) {
        if let (Some(a_rep), Some(b_rep)) = (self.find_representative(a), self.find_representative(b)) {
            if a_rep != b_rep {
                self.size -= 1;
                if self.nodes[a_rep].rank < self.nodes[b_rep].rank {
                    self.set_parent(a_rep, b_rep);
                    self.nodes[b_rep].size += self.nodes[a_rep].size;
                    self.representatives.remove(&a_rep);
                } else {
                    self.set_parent(b_rep, a_rep);
                    self.nodes[a_rep].size += self.nodes[b_rep].size;
                    self.representatives.remove(&b_rep);
                    if self.nodes[a_rep].rank == self.nodes[b_rep].rank {
                        self.nodes[a_rep].rank += 1;
                    }
                }
            }
        }
    }

    fn find_representative(&mut self, mut a: usize) -> Option<usize> {
        if let Some(mut parent) = self.parent(a) {
            while a != parent {
                let grandparent = self.parent(parent).unwrap();
                self.set_parent(a, grandparent);
                (a, parent) = (parent, grandparent);
            }
            Some(a)
        } else {
            None
        }
    }

    fn parent(&self, a: usize) -> Option<usize> {
        self.nodes.get(a).map(|node| node.parent)
    }

    fn set_parent(&mut self, a: usize, parent: usize) {
        self.nodes.get_mut(a).map(|p| p.parent = parent);
    }
}

struct UnionFindNode {
    rank: usize,
    size: usize,
    parent: usize,
}

impl UnionFindNode {
    fn new_at_index(index: usize) -> Self {
        Self { rank: 0, size: 1, parent: index }
    }
}
