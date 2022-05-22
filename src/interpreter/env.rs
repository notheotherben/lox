use std::{collections::HashMap, rc::Rc, sync::RwLock};

use crate::{errors, LoxError};

use super::Value;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RwLock<Environment>>>,
}

impl Environment {
    pub fn child(parent: Rc<RwLock<Environment>>) -> Rc<RwLock<Self>> {
        Rc::new(RwLock::new(Self { enclosing: Some(parent), ..Default::default() }))
    }

    pub fn define<K: Into<String>>(&mut self, key: K, value: Value) {
        self.values.insert(key.into(), value);
    }

    pub fn assign<K: Into<String>>(&mut self, key: K, value: Value) -> Result<(), LoxError> {
        let key = key.into();
        if let std::collections::hash_map::Entry::Occupied(mut e) = self.values.entry(key.clone()) {
            e.insert(value);
            Ok(())
        } else if let Some(ref mut env) = self.enclosing {
            env.write().unwrap().assign(key, value)
        } else {
            Err(errors::user(
                &format!("Attempted to assign to an undefined variable '{}'", &key),
                &format!("Try defining the variable instead using `var {} = {}`.", &key, value),
            ))
        }
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.values.get(key).cloned().or_else(|| self.enclosing.as_ref().and_then(|e| e.read().unwrap().get(key)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global() {
        let mut env = Environment::default();
        env.define("a", Value::Number(1.0));
        env.define("b", Value::Number(2.0));

        assert_eq!(env.get("a"), Some(Value::Number(1.0)));
        assert_eq!(env.get("b"), Some(Value::Number(2.0)));
        assert_eq!(env.get("c"), None);
    }

    #[test]
    fn test_scoped() {
        let global = Rc::new(RwLock::new(Environment::default()));
        global.write().unwrap().define("a", Value::Number(1.0));
        global.write().unwrap().define("b", Value::Number(2.0));

        let env = Environment::child(Rc::clone(&global));
        assert_eq!(env.read().unwrap().get("a"), Some(Value::Number(1.0)));

        env.write().unwrap().define("a", Value::Number(3.0));
        env.write().unwrap().define("c", Value::Number(4.0));

        assert_eq!(global.read().unwrap().get("a"), Some(Value::Number(1.0)));


        assert_eq!(env.read().unwrap().get("a"), Some(Value::Number(3.0)));
        assert_eq!(env.read().unwrap().get("b"), Some(Value::Number(2.0)));
        assert_eq!(env.read().unwrap().get("c"), Some(Value::Number(4.0)));
        assert_eq!(env.read().unwrap().get("d"), None);
    }
}