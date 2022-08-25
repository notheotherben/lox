use std::{rc::Rc, cell::{RefCell, Ref, RefMut}};

use super::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd)]
pub struct Object<T> {
    value: RefCell<T>,
    marked: RefCell<bool>,
}

pub trait Collector {
    fn alloc(&self, value: Value) -> Rc<Object<Value>>;
    fn mark(&self, object: Rc<Object<Value>>);
}

pub trait Collectible {
    fn mark(&self, gc: &dyn Collector);
}

pub struct GC {
    heap: RefCell<Vec<Rc<Object<Value>>>>,
    grey_stack: RefCell<Vec<Rc<Object<Value>>>>,
}

impl GC {
    pub fn new() -> Self {
        Self {
            heap: RefCell::new(Vec::new()),
            grey_stack: RefCell::new(Vec::new()),
        }
    }

    pub fn collect(&self, root: &dyn Collectible) {
        self.prepare();
        root.mark(self);
        self.scan();
        self.sweep();
    }

    fn prepare(&self) {
        self.heap.borrow().iter().for_each(|object| {
            object.marked.replace(false);
        });
    }

    fn scan(&self) {
        loop {
            let grey = self.grey_stack.borrow_mut().pop();
            if grey.is_none() {
                break;
            }

            let grey = grey.unwrap();
            grey.mark(self);
        }
    }

    fn sweep(&self) {
        self.heap.borrow_mut().retain(|object| *object.marked.borrow());
    }
}

impl Collector for GC {
    fn alloc(&self, value: Value) -> Rc<Object<Value>> {
        let object = Rc::new(Object::new(value));
        self.heap.borrow_mut().push(object.clone());
        object
    }

    fn mark(&self, object: Rc<Object<Value>>) {
        if !*object.marked.borrow() {
            object.marked.replace(true);
            self.grey_stack.borrow_mut().push(object);
        }
    }
}

impl<T> Object<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: RefCell::new(value),
            marked: RefCell::new(false),
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
    fn mark(&self, gc: &dyn Collector) {
        self.value().mark(gc);
    }
}
