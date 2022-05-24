use std::{collections::HashMap, fmt::{Debug, Display}, rc::Rc, sync::Mutex};

use crate::{LoxError, lexer::Token, errors};

use super::{Fun, Value};

#[derive(Clone, PartialEq)]
pub struct Class {
    name: String,
    methods: HashMap<String, Fun>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S) -> Self {
        Self { name: name.into(), methods: Default::default() }
    }

    pub fn define<S: Into<String>>(&mut self, name: S, fun: Fun) {
        self.methods.insert(name.into(), fun);
    }

    pub fn find_method(&self, name: &str) -> Option<Fun> {
        self.methods.get(name).cloned()
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

#[derive(Clone)]
pub struct Instance {
    class: Rc<Class>,
    props: Rc<Mutex<HashMap<String, Value>>>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Self { class, props: Rc::new(Mutex::new(Default::default())) }
    }

    pub fn get(&self, property: &Token) -> Result<Value, LoxError> {
        self.props.lock().unwrap().get(property.lexeme()).cloned().or_else(|| {
            self.class.find_method(property.lexeme()).map(|fun| Value::Function(fun.bind(Value::Instance(self.clone()))))
        }).ok_or_else(|| errors::user(
            &format!("{} does not have a property `{}` at {}.", self.class, property.lexeme(), property.location()),
            "Make sure that you are attempting to access a property which exists on this instance."
        ))
    }

    pub fn set(&mut self, property: &Token, value: Value) -> Result<(), LoxError> {
        self.props.lock().unwrap().insert(property.lexeme().to_string(), value);
        Ok(())
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.class, &other.class) && Rc::ptr_eq(&self.props, &other.props)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", &self.class)
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", &self.class)
    }
}