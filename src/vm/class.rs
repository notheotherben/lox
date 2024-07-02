use std::mem::size_of_val;
use std::{collections::HashMap, mem::size_of};
use std::rc::Rc;
use std::fmt::Display;

use super::{Alloc, Collectible, Function, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: Rc<String>,
    pub statics: HashMap<String, Alloc<Function>>,
    pub methods: HashMap<String, Alloc<Function>>,
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
    fn gc(&self) {
        for fun in self.methods.values() {
            fun.gc();
        }
        
        for fun in self.statics.values() {
            fun.gc();
        }
    }

    fn size(&self) -> usize {
        size_of::<Self>() +
        size_of_val(&self.name) +
        size_of_val(&self.statics) +
        size_of_val(&self.methods)
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
    pub class: Alloc<Class>,
    pub fields: HashMap<String, Alloc<Value>>,
}

impl Instance {
    pub fn new(class: Alloc<Class>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

impl Collectible for Instance {
    fn gc(&self) {
        self.class.gc();

        for field in self.fields.values() {
            field.gc();
        }
    }

    fn size(&self) -> usize {
        size_of::<Self>() +
        size_of_val(&self.class) +
        size_of_val(&self.fields)
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