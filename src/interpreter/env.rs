use std::{collections::HashMap, rc::Rc, sync::RwLock};

use crate::{ast::Literal, errors, LoxError};

#[derive(Debug, Clone, Default)]
pub struct Environment {
    values: HashMap<String, Literal>,
    enclosing: Option<Rc<RwLock<Environment>>>,
}

impl Environment {
    pub fn child(parent: Rc<RwLock<Environment>>) -> Self {
        Self { enclosing: Some(parent), ..Default::default() }
    }

    pub fn define<K: Into<String>>(&mut self, key: K, value: Literal) {
        self.values.insert(key.into(), value);
    }

    pub fn assign<K: Into<String>>(&mut self, key: K, value: Literal) -> Result<(), LoxError> {
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

    pub fn get(&self, key: &str) -> Option<Literal> {
        self.values.get(key).cloned().or_else(|| self.enclosing.as_ref().and_then(|e| e.read().unwrap().get(key)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global() {
        let mut env = Environment::default();
        env.define("a", Literal::Number(1.0));
        env.define("b", Literal::Number(2.0));

        assert_eq!(env.get("a"), Some(Literal::Number(1.0)));
        assert_eq!(env.get("b"), Some(Literal::Number(2.0)));
        assert_eq!(env.get("c"), None);
    }

    #[test]
    fn test_scoped() {
        let mut global = Rc::new(RwLock::new(Environment::default()));
        global.write().unwrap().define("a", Literal::Number(1.0));
        global.write().unwrap().define("b", Literal::Number(2.0));

        let mut env = Environment::child(Rc::clone(&global));
        assert_eq!(env.get("a"), Some(Literal::Number(1.0)));

        env.define("a", Literal::Number(3.0));
        env.define("c", Literal::Number(4.0));

        assert_eq!(global.read().unwrap().get("a"), Some(Literal::Number(1.0)));


        assert_eq!(env.get("a"), Some(Literal::Number(3.0)));
        assert_eq!(env.get("b"), Some(Literal::Number(2.0)));
        assert_eq!(env.get("c"), Some(Literal::Number(4.0)));
        assert_eq!(env.get("d"), None);
    }
}