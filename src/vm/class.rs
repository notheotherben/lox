use std::rc::Rc;

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