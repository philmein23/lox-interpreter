use std::collections::HashMap;

use crate::object::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Object>,
    pub enclosing: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn extend(enclosing: Environment) -> Environment {
        Environment {
            values: HashMap::new(),
            enclosing: Some(Box::new(enclosing)),
        }
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        match self.values.get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.enclosing {
                Some(env) => {
                    let value = env.as_ref().get(name);
                    value
                }
                None => None,
            },
        }
    }

    pub fn assign(&mut self, name: String, value: Object) {
        match self.values.contains_key(&name) {
            true => self.define(name, value),
            false => match &mut self.enclosing {
                Some(enclosing) => {
                    enclosing.assign(name, value);
                }
                None => panic!("attempting to assign undeclared variable"),
            },
        }
    }
}
