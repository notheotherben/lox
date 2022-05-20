use std::collections::HashMap;

use crate::ast::Literal;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn define<K: Into<String>>(&mut self, key: K, value: Literal) {
        self.values.insert(key.into(), value);
    }

    pub fn get(&self, key: &str) -> Option<&Literal> {
        self.values.get(key)
    }
}