use itertools::Itertools;

struct ListNode<T> {
    val: T,
    prev: Option<usize>,
    next: Option<usize>,
}

#[derive(Copy, Clone)]
pub struct ListItemId(usize);

pub struct LinkedList<T> {
    nodes: Vec<ListNode<T>>,
    head: Option<usize>,
    tail: Option<usize>,
    length: usize,
}

impl<T> LinkedList<T> {
    pub fn from<I: IntoIterator<Item = T>>(items: I) -> Self {
        let mut nodes = items.into_iter().enumerate().map(|(idx, item)| ListNode {
            val: item,
            prev: idx.checked_sub(1),
            next: idx.checked_add(1),
        }).collect_vec();
        if let Some(node) = nodes.last_mut() { node.next = None; }
        Self { 
            head: if nodes.len() > 0 { Some(0) } else { None },
            tail: if nodes.len() > 0 { Some(nodes.len() - 1) } else { None },
            length: nodes.len(),
            nodes, 
        }
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn head(&self) -> Option<ListItemId> {
        self.head.map(ListItemId)
    }

    pub fn tail(&self) -> Option<ListItemId> {
        self.tail.map(ListItemId)
    }

    pub fn val(&self, ListItemId(id): ListItemId) -> &T {
        &self.nodes[id].val
    }

    pub fn next(&self, ListItemId(id): ListItemId) -> Option<ListItemId> {
        self.nodes[id].next.map(ListItemId)
    }

    pub fn prev(&self, ListItemId(id): ListItemId) -> Option<ListItemId> {
        self.nodes[id].prev.map(ListItemId)
    }

    pub fn push_back(&mut self, item: T) {
        self.nodes.push(ListNode { val: item, prev: self.tail, next: None });
        if self.head.is_none() {
            self.head = Some(self.nodes.len() - 1);
            self.tail = self.head;
        } else {
            self.nodes[self.tail.unwrap()].next = Some(self.nodes.len() - 1);
            self.tail = Some(self.nodes.len() - 1);
        }
    }

    pub fn remove(&mut self, ListItemId(id): ListItemId) {
        if let Some(prev) = self.nodes[id].prev { self.nodes[prev].next = self.nodes[id].next; }
        if let Some(next) = self.nodes[id].next { self.nodes[next].prev = self.nodes[id].prev; }
        if self.head == Some(id) { self.head = self.nodes[id].next; }
        if self.tail == Some(id) { self.tail = self.nodes[id].prev; }
        self.length -= 1;
    }
}
