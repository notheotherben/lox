#[derive(Default, Debug, Clone)]
pub struct Stack<T> {
    items: Vec<T>,
    offset: usize,
}

impl<T> Stack<T> {
    pub fn len(&self) -> usize {
        self.items.len() - self.offset
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    pub fn get(&self, index: usize) -> Option<&T> {
        self.items.get(self.offset + index)
    }

    pub fn set(&mut self, index: usize, value: T) {
        self.items[self.offset + index] = value;
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
           self.items.pop()
        }
    }

    pub fn peek(&self) -> Option<&T> {
        self.items.last()
    }

    pub fn truncate(&mut self, len: usize) {
        self.items.truncate(len + self.offset);
    }
}
