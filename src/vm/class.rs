use std::{rc::Rc, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Class {
    pub name: Rc<String>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Class {
            name: Rc::new(name.into()),
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Instance {
    pub class: Rc<Class>,
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} instance", self.class)
    }
}