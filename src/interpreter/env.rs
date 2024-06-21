use std::{collections::HashMap, rc::Rc, sync::Mutex};

use crate::{errors, LoxError, lexer::Token};

use super::Value;

#[derive(Clone, Debug, Default)]
pub struct Environment(Rc<Mutex<Scope>>);

#[derive(Debug, Clone, Default)]
pub struct Scope{
    values: HashMap<String, Value>,
    parent: Option<Rc<Mutex<Scope>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn branch(&self) -> Environment {
        let child = Scope {
            parent: Some(self.0.clone()),
            ..Default::default()
        };

        Environment(Rc::new(Mutex::new(child)))
    }

    pub fn define(&mut self, key: &Token, value: Value) {
        self.0.lock().unwrap().define(key, value);
    }

    pub fn assign(&mut self, key: &Token, value: Value) -> Result<(), LoxError> {
        self.0.lock().unwrap().assign(key, value)
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.0.lock().unwrap().get(key)
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Scope {
    pub fn define(&mut self, key: &Token, value: Value) {
        self.values.insert(key.lexeme().to_string(), value);
    }

    pub fn assign(&mut self, key: &Token, value: Value) -> Result<(), LoxError> {
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.values.entry(key.lexeme().to_string()) {
            e.insert(value);
            Ok(())
        } else if let Some(ref mut env) = self.parent {
            env.lock().unwrap().assign(key, value)
        } else {
            Err(errors::runtime(
                key.location(),
                format!("Attempted to assign to an undefined variable '{}'", &key),
                format!("Try defining the variable instead using `var {} = {}`.", &key, value),
            ))
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.values.get(key).cloned().or_else(|| self.parent.as_ref().and_then(|p| p.lock().unwrap().get(key)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn id(key: &str) -> Token {
        Token::Identifier(1.into(), key.to_string())
    }

    #[test]
    fn test_global() {
        let mut env = Environment::new();
        env.define(&id("a"), Value::Number(1.0));
        env.define(&id("b"), Value::Number(2.0));

        assert_eq!(env.get("a"), Some(Value::Number(1.0)));
        assert_eq!(env.get("b"), Some(Value::Number(2.0)));
        assert_eq!(env.get("c"), None);
    }

    #[test]
    fn test_scoped() {
        let mut global = Environment::new();
        global.define(&id("a"), Value::Number(1.0));
        global.define(&id("b"), Value::Number(2.0));
        let mut env = global.branch();

        assert_eq!(env.get("a"), Some(Value::Number(1.0)));

        env.define(&id("a"), Value::Number(3.0));
        env.define(&id("c"), Value::Number(4.0));

        assert_eq!(global.get("a"), Some(Value::Number(1.0)));

        assert_eq!(env.get("a"), Some(Value::Number(3.0)));
        assert_eq!(env.get("b"), Some(Value::Number(2.0)));
        assert_eq!(env.get("c"), Some(Value::Number(4.0)));
        assert_eq!(env.get("d"), None);
    }
}