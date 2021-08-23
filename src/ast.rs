use core::fmt;
use std::fmt::Display;

use crate::token::Token;

pub enum Expression {
    Binary(Box<Expression>, Infix, Box<Expression>),
    Unary(Prefix, Box<Expression>),
    Grouping(Box<Expression>),
    Number(i64),
    StringLiteral(String),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Binary(left, op, right) => {
                write!(f, "({} {} {})", left, op, right)
            }
            Expression::Unary(_, _) => todo!(),
            Expression::Grouping(_) => todo!(),
            Expression::Number(_) => todo!(),
            Expression::StringLiteral(_) => todo!(),
            _ => todo!(),
        }
    }
}

pub enum Prefix {
    BANG,
    MINUS,
}

pub enum Infix {
    PLUS,
    MINUS,
    STAR,
    SLASH,
}
