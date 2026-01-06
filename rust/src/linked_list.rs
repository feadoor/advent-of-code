use itertools::Itertools;

pub struct ListItemId(usize);

pub struct LinkedList<T> {
    nodes: Vec<ListNode<T>>,
    head: Option<usize>,
    tail: Option<usize>,
    length: usize,
}

impl<T> LinkedList<T> {

    pub fn from<I: IntoIterator<Item = T>>(items: I) -> Self {
        let mut nodes = items.into_iter().enumerate().map(|(idx, item)| 
            ListNode::new(item, idx.checked_sub(1), idx.checked_add(1))
        ).collect_vec();
        if let Some(node) = nodes.last_mut() { node.next = None; }
        Self { 
            head: (nodes.len() > 0).then_some(0),
            tail: (nodes.len() > 0).then_some(nodes.len() - 1),
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

    pub fn val(&self, &ListItemId(id): &ListItemId) -> &T {
        &self.nodes[id].val
    }

    pub fn next(&self, &ListItemId(id): &ListItemId) -> Option<ListItemId> {
        self.nodes[id].next.map(ListItemId)
    }

    pub fn prev(&self, &ListItemId(id): &ListItemId) -> Option<ListItemId> {
        self.nodes[id].prev.map(ListItemId)
    }

    pub fn next_circular(&self, id: &ListItemId) -> ListItemId {
        self.next(id).or(self.head.map(ListItemId)).unwrap()
    }

    pub fn prev_circular(&self, id: &ListItemId) -> ListItemId {
        self.prev(id).or(self.tail.map(ListItemId)).unwrap()
    }

    pub fn insert_after(&mut self, &ListItemId(id): &ListItemId, val: T) -> ListItemId {
        self.nodes.push(ListNode::new(val, Some(id), self.nodes[id].next));
        if let Some(next) = self.nodes[id].next { self.nodes[next].prev = Some(self.nodes.len() - 1); }
        else { self.tail = Some(self.nodes.len() - 1); }
        self.nodes[id].next = Some(self.nodes.len() - 1);
        ListItemId(self.nodes.len() - 1)
    }

    pub fn remove(&mut self, ListItemId(id): ListItemId) {
        if let Some(prev) = self.nodes[id].prev { self.nodes[prev].next = self.nodes[id].next; }
        if let Some(next) = self.nodes[id].next { self.nodes[next].prev = self.nodes[id].prev; }
        if self.head == Some(id) { self.head = self.nodes[id].next; }
        if self.tail == Some(id) { self.tail = self.nodes[id].prev; }
        self.length -= 1;
    }
}

struct ListNode<T> {
    val: T,
    prev: Option<usize>,
    next: Option<usize>,
}

impl<T> ListNode<T> {
    fn new(val: T, prev: Option<usize>, next: Option<usize>) -> Self {
        Self { val, prev, next }
    }
}
