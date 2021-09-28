use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Nil,
    Boolean(bool),
    Number(i64),
    String(String),
    Function(String),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Object::Nil => write!(f, "nil"),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
            Object::Function(s) => write!(f, "{}", s),
        }
    }
}
