use std::collections::HashMap;
use std::rc::Rc;
use std::fmt::Display;

use super::{Alloc, Collectible, Function, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: Rc<String>,
    pub statics: HashMap<String, Rc<Function>>,
    pub methods: HashMap<String, Rc<Function>>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: Rc::new(name.into()),
            methods: HashMap::new(),
            statics: HashMap::new(),
        }
    }
}

impl Collectible for Class {
    fn mark(&self, gc: &mut super::GC) {
        for fun in self.methods.values() {
            fun.mark(gc);
        }
        
        for fun in self.statics.values() {
            fun.mark(gc);
        }
    }
}

impl PartialOrd for Class {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Alloc<Value>>,
}

impl Collectible for Instance {
    fn mark(&self, gc: &mut super::GC) {
        self.class.mark(gc);

        for field in self.fields.values() {
            field.mark(gc);
        }
    }
}

impl PartialOrd for Instance {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        None
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}