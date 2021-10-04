use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(i64),
    String(String),
    LoxFunction(String, u64),
    LoxClass(String, u64),
    LoxInstance(String, u64),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
            Object::LoxFunction(s, _) => write!(f, "{}", s),
            Object::LoxClass(s, _) => write!(f, "{}", s),
            Object::LoxInstance(s, _) => write!(f, "{}", s),
        }
    }
}
