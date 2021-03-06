use core::fmt;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Print(Box<Expression>),
    Expression(Box<Expression>),
    Var(String, Option<Box<Expression>>),
    Block(Vec<Statement>),
    If(Box<Expression>, Box<Statement>, Option<Box<Statement>>),
    While(Box<Expression>, Box<Statement>),
    Function(String, Vec<String>, Vec<Statement>),
    Return(Box<Expression>),
    Class(String, Option<Box<Expression>>, Vec<Statement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Print(expr) => write!(f, "print {}", expr),
            Statement::Expression(expr) => write!(f, "{}", expr),
            Statement::Var(name, expr) => match expr {
                Some(e) => write!(f, "var {} = {}", name, e),
                None => write!(f, "var {}", name),
            },
            Statement::Block(stmts) => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Statement::If(cond, then, els) => match els {
                Some(e) => write!(f, "if ({}) {}\n else {}", cond, then, e),
                None => write!(f, "if ({}) {}", cond, then),
            },
            Statement::While(cond, body) => {
                write!(f, "while ({}) {}", cond, body)
            }
            Statement::Function(name, params, body) => {
                let params_joined = params.join(",");
                write!(f, "fun {}({}) {{\n", name, params_joined)?;
                for stmt in body {
                    write!(f, "{}\n", stmt)?;
                }
                write!(f, "}}")
            }
            Statement::Return(value) => {
                write!(f, "return {}", value)
            }
            Statement::Class(name, super_class, methods) => {
                if super_class.is_some() {
                    write!(f, "class {} < {:?} {{\n", name, super_class)?;
                    for method in methods {
                        write!(f, "{}\n", method)?;
                    }
                    write!(f, "}}")
                } else {
                    write!(f, "class {} {{\n", name)?;

                    for method in methods {
                        write!(f, "{}\n", method)?;
                    }
                    write!(f, "}}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Binary(Box<Expression>, Infix, Box<Expression>),
    Unary(Prefix, Box<Expression>),
    Grouping(Box<Expression>),
    Number(i64),
    StringLiteral(String),
    Boolean(bool),
    Variable(String),
    Assign(String, Box<Expression>),
    Logical(Box<Expression>, Infix, Box<Expression>),
    Call(Box<Expression>, Vec<Box<Expression>>),
    Get(Box<Expression>, String),
    Set(Box<Expression>, String, Box<Expression>),
    This(),
    Super(String),
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
            Expression::Variable(name) => {
                write!(f, "{}", name)
            }
            Expression::Assign(name, expr) => {
                write!(f, "{} = {}", name, expr)
            }
            Expression::Logical(left, logical_op, right) => {
                write!(f, "({} {} {})", logical_op, left, right)
            }
            Expression::Call(callee, arguments) => {
                let comma_joined: String = arguments
                    .iter()
                    .map(|arg| arg.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                write!(f, "{}({})", callee, comma_joined)
            }
            Expression::Get(object, prop) => {
                write!(f, "{}.{}", object, prop)
            }
            Expression::Set(object, prop, value) => {
                write!(f, "{}.{}={}", object, prop, value)
            }
            Expression::This() => {
                write!(f, "this")
            }
            Expression::Super(method) => {
                write!(f, "super.{}", method)
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
    AND,
    OR,
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
            Infix::AND => write!(f, "and"),
            Infix::OR => write!(f, "or"),
        }
    }
}
