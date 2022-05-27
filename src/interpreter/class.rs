use std::{collections::HashMap, fmt::{Debug, Display}, rc::Rc, sync::Mutex};

use crate::{LoxError, lexer::Token, errors};

use super::{Fun, Value};

#[derive(Clone, PartialEq)]
pub struct Class {
    name: String,
    superclass: Option<Rc<Class>>,
    methods: HashMap<String, Fun>,
    statics: HashMap<String, Value>,
}

impl Class {
    pub fn new<S: Into<String>>(name: S, superclass: Option<Rc<Class>>) -> Self {
        Self { name: name.into(), superclass, methods: Default::default(), statics: Default::default() }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn define<S: Into<String>>(&mut self, name: S, fun: Fun) {
        self.methods.insert(name.into(), fun);
    }

    pub fn find_method(&self, name: &str) -> Option<Fun> {
        self.methods.get(name).cloned().or_else(|| self.superclass.clone().and_then(|s| s.find_method(name)))
    }

    pub fn set<S: Into<String>>(&mut self, name: S, value: Value) {
        self.statics.insert(name.into(), value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.statics.get(name).cloned()
    }

    pub fn superclass(&self) -> Option<Rc<Class>> {
        self.superclass.clone()
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &self.name)
    }
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", &self.name)
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
            self.class.find_method(property.lexeme()).map(|fun| Value::Function(fun.bind(self.clone(), property.location())))
        }).ok_or_else(|| errors::runtime(
            property.location(),
            &format!("{} does not have a property `{}`.", self.class, property.lexeme()),
            "Make sure that you are attempting to access a property which exists on this instance.",
        ))
    }

    pub fn set(&mut self, property: &Token, value: Value) -> Result<(), LoxError> {
        self.props.lock().unwrap().insert(property.lexeme().to_string(), value);
        Ok(())
    }

    pub fn class(&self) -> Rc<Class> {
        self.class.clone()
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.class, &other.class) && Rc::ptr_eq(&self.props, &other.props)
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} instance", &self.class)
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} instance", &self.class)
    }
}