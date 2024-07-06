use std::mem::{size_of, size_of_val};
use std::rc::Rc;
use std::fmt::Display;

use fnv::FnvHashMap;

use super::{Alloc, Collectible, Function, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: Rc<String>,
    pub statics: FnvHashMap<String, Alloc<Function>>,
    pub methods: FnvHashMap<String, Alloc<Function>>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self {
            name: Rc::new(name.into()),
            methods: FnvHashMap::default(),
            statics: FnvHashMap::default(),
        }
    }

    pub fn inherit(&mut self, superclass: &Class) {
        for (name, method) in &superclass.methods {
            self.methods.insert(name.clone(), *method);
        }

        for (name, method) in &superclass.statics {
            self.statics.insert(name.clone(), *method);
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
    pub fields: FnvHashMap<String, Alloc<Value>>,
}

impl Instance {
    pub fn new(class: Alloc<Class>) -> Self {
        Self {
            class,
            fields: FnvHashMap::default(),
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