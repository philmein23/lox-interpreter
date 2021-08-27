use crate::ast::{Expression, Infix, Prefix, Statement};
use crate::object::Object;
pub struct Interpreter {}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
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

    pub fn evaluate(&self, stmts: Vec<Statement>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            match stmt {
                Statement::Expression(expr) => {
                    let _value = self.evaluate_expression(*expr)?;
                }
                Statement::Print(expr) => {
                    self.evaluate_print_statement(*expr);
                }
            }
        }

        Ok(())
    }

    fn evaluate_print_statement(&self, expr: Expression) {
        let value = self.evaluate_expression(expr).unwrap();
        println!("{}", value);
    }

    fn evaluate_expression(&self, expr: Expression) -> Result<Object, RuntimeError> {
        match expr {
            Expression::Binary(left, operator, right) => {
                let left = self.evaluate_expression(*left)?;
                let right = self.evaluate_expression(*right)?;
                self.eval_infix_expression(operator, left, right)
            }
            Expression::Number(value) => Ok(Object::Number(value)),
            Expression::Boolean(value) => Ok(Object::Boolean(value)),
            Expression::StringLiteral(value) => Ok(Object::String(value)),
            Expression::Unary(operator, right) => {
                let right = self.evaluate_expression(*right)?;
                self.eval_prefix_expression(operator, right)
            }
            Expression::Grouping(expr) => self.evaluate_expression(*expr),
            Expression::Nil => Ok(Object::Nil),
            _ => Err(RuntimeError::InvalidSyntax),
        }
    }

    fn eval_prefix_expression(
        &self,
        operator: Prefix,
        right: Object,
    ) -> Result<Object, RuntimeError> {
        match operator {
            Prefix::BANG => match right {
                Object::Boolean(value) => Ok(Object::Boolean(!value)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be a boolean type".into(),
                )),
            },
            Prefix::MINUS => match right {
                Object::Number(value) => Ok(Object::Number(-value)),
                _ => Err(RuntimeError::NewRuntimeError(
                    "Expected operand to be number type".into(),
                )),
            },
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
