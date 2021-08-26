use crate::ast::{Expression, Infix, Prefix};
use crate::object::Object;
pub struct Interpreter {}

enum RuntimeError {
    NewRuntimeError(String),
    InvalidSyntax,
    InvalidArguments,
    InvalidNumberOfArguments,
    InvalidNumberOfArgumentsForFunction,
    InvalidNumberOfArgumentsForMethod,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {}
    }

    pub fn evaluate(&self, expr: Expression) -> Result<Object, RuntimeError> {
        match expr {
            Expression::Binary(left, operator, right) => {
                let left = self.evaluate(*left)?;
                let right = self.evaluate(*right)?;
                self.eval_infix_expression(operator, left, right)
            }
            _ => Err(RuntimeError::InvalidSyntax),
        }
    }

    fn eval_infix_expression(
        &self,
        operator: Infix,
        left: Object,
        right: Object,
    ) -> Result<Object, RuntimeError> {
        match operator {
            Infix::PLUS => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Number(left + right)),
                (Object::String(s1), Object::String(s2)) => {
                    Ok(Object::String(format!("{}{}", s1, s2)))
                }
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be of same type - string or number.".into(),
                )),
            },
            Infix::MINUS => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Number(left - right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be a number type".into(),
                )),
            },
            Infix::SLASH => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Number(left / right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be a number type".into(),
                )),
            },
            Infix::STAR => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Number(left * right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
            Infix::GREATER => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Boolean(left > right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
            Infix::GREATER_EQUAL => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Boolean(left >= right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
            Infix::LESS => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Boolean(left < right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
            Infix::LESS_EQUAL => match (left, right) {
                (Object::Number(left), Object::Number(right)) => Ok(Object::Boolean(left <= right)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
            Infix::BANG_EQUAL => Ok(Object::Boolean(!(left == right))),
            Infix::EQUAL_EQUAL => Ok(Object::Boolean(left == right)),
        }
    }
}
