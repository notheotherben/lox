use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Class {
    pub name: String,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Class {
            name: name.into(),
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}