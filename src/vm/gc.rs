use std::{cell::{Cell, Ref, RefCell, RefMut}, rc::Rc};

use super::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub struct Object<T> {
    value: RefCell<T>,
    marked: Cell<bool>,
}

pub trait Collector {
    fn alloc(&mut self, value: Value) -> Rc<Object<Value>>;
    fn mark(&mut self, object: Rc<Object<Value>>);
}

pub trait Collectible {
    fn mark(&self, gc: &mut dyn Collector);
}

pub struct GC {
    heap: Vec<Rc<Object<Value>>>,
    grey_stack: Vec<Rc<Object<Value>>>,
}

impl GC {
    pub fn new() -> Self {
        Self {
            heap: Vec::new(),
            grey_stack: Vec::new(),
        }
    }

    pub fn collect<M>(&mut self, marker: M)
        where M: FnOnce(&mut dyn Collector)
    {
        self.prepare();
        marker(self);
        self.scan();
        self.sweep();
    }

    fn prepare(&self) {
        self.heap.iter().for_each(|object| {
            object.marked.replace(false);
        });
    }

    fn scan(&mut self) {
        while let Some(grey) = self.grey_stack.pop() {
            grey.mark(self);
        }
    }

    fn sweep(&mut self) {
        self.heap.retain(|object| object.marked.get());
    }
}

impl Collector for GC {
    fn alloc(&mut self, value: Value) -> Rc<Object<Value>> {
        let object = Rc::new(Object::new(value));
        self.heap.push(object.clone());
        object
    }

    fn mark(&mut self, object: Rc<Object<Value>>) {
        if !object.marked.get() {
            object.marked.set(true);
            self.grey_stack.push(object);
        }
    }
}

impl<T> Object<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: RefCell::new(value),
            marked: Cell::new(false),
        }
    }

    pub fn value(&self) -> Ref<T> {
        self.value.borrow()
    }

    pub fn value_mut(&self) -> RefMut<T> {
        self.value.borrow_mut()
    }

    pub fn replace(&self, value: T) {
        *self.value.borrow_mut() = value;
    }
}

impl<T: Collectible> Collectible for Object<T> {
    fn mark(&self, gc: &mut dyn Collector) {
        self.value().mark(gc);
    }
}
