use core::fmt;
use std::fmt::Display;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binary(Box<Expression>, Infix, Box<Expression>),
    Unary(Prefix, Box<Expression>),
    Grouping(Box<Expression>),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    Nil,
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Binary(left, op, right) => {
                write!(f, "({} {} {})", op, left, right)
            }
            Expression::Unary(op, exp) => {
                write!(f, "({}{})", op, exp)
            }
            Expression::Grouping(expr) => {
                write!(f, "({})", expr)
            }
            Expression::Number(num) => {
                write!(f, "{}", num)
            }
            Expression::StringLiteral(word) => {
                write!(f, "\"{}\"", word)
            }
            Expression::Boolean(b) => {
                write!(f, "{}", b)
            }
            Expression::Nil => {
                write!(f, "nil")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prefix {
    BANG,
    MINUS,
}

impl Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::BANG => write!(f, "!"),
            Prefix::MINUS => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Infix {
    PLUS,
    MINUS,
    STAR,
    SLASH,
    EQUAL_EQUAL,
    BANG_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
}

impl Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::PLUS => write!(f, "+"),
            Infix::MINUS => write!(f, "-"),
            Infix::STAR => write!(f, "*"),
            Infix::SLASH => write!(f, "/"),
            Infix::EQUAL_EQUAL => write!(f, "=="),
            Infix::BANG_EQUAL => write!(f, "!="),
            Infix::GREATER => write!(f, ">"),
            Infix::GREATER_EQUAL => write!(f, ">="),
            Infix::LESS => write!(f, "<"),
            Infix::LESS_EQUAL => write!(f, "<="),
        }
    }
}
