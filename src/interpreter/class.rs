use std::{collections::HashMap, fmt::{Debug, Display}};

use super::Fun;

#[derive(Clone, PartialEq)]
pub struct Class {
    name: String,
    methods: HashMap<String, Fun>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self { name: name.into(), methods: Default::default() }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", &self.name)
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", &self.name)
    }
}