use std::collections::HashMap;

use crate::object::Object;

#[derive(Clone, Debug, PartialEq)]
pub struct Environment {
    values: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Object) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        self.values.get(name).map(|val| val.clone())
    }
}
