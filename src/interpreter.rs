use std::borrow::BorrowMut;
use std::vec;

use crate::ast::{Expression, Infix, Prefix, Statement};
use crate::environment::Environment;
use crate::object::Object;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    NewRuntimeError(String),
    InvalidSyntax,
    InvalidArguments,
    InvalidNumberOfArguments,
    InvalidNumberOfArgumentsForFunction,
    InvalidNumberOfArgumentsForMethod,
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let env = Environment::new();
        Interpreter { env }
    }

    pub fn evaluate(&mut self, stmts: Vec<Statement>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            match stmt {
                Statement::Var(name, expr) => match expr {
                    Some(expr) => {
                        let value = self.evaluate_expression(*expr)?;
                        self.env.define(name, value);
                    }
                    None => self.env.define(name, Object::Nil),
                },
                Statement::Expression(expr) => {
                    let _value = self.evaluate_expression(*expr)?;
                }
                Statement::Print(expr) => {
                    self.evaluate_print_statement(*expr);
                }
                Statement::Block(stmts) => {
                    let nested_env = Environment::extend(self.env.clone());
                    self.execute_block(stmts, nested_env);
                }
                Statement::If(cond, then, alt) => match alt {
                    Some(alt) => {
                        self.evaluate_if_statement(*cond, *then, Some(*alt));
                    }
                    None => {
                        self.evaluate_if_statement(*cond, *then, None);
                    }
                },
            }
        }

        Ok(())
    }

    fn is_truthy(&self, val: Object) -> bool {
        match val {
            Object::Nil => false,
            Object::Boolean(false) => false,
            Object::Boolean(true) => true,
            _ => true,
        }
    }

    fn evaluate_if_statement(&mut self, cond: Expression, then: Statement, alt: Option<Statement>) {
        let evaluated = self.evaluate_expression(cond).unwrap();
        let mut statements = vec![];

        match self.is_truthy(evaluated) {
            true => {
                statements.push(then);
                self.evaluate(statements);
            }
            false => {
                if let Some(alt) = alt {
                    statements.push(alt);
                }

                self.evaluate(statements);
            }
        }
    }

    fn execute_block(&mut self, statements: Vec<Statement>, env: Environment) {
        let previous_env = self.env.clone();
        self.env = env;
        let _ = self.evaluate(statements);
        self.env = previous_env;
    }

    fn evaluate_print_statement(&mut self, expr: Expression) {
        let value = self.evaluate_expression(expr).unwrap();
        println!("{}", value);
    }

    fn evaluate_expression(&mut self, expr: Expression) -> Result<Object, RuntimeError> {
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
            Expression::Variable(name) => {
                let value = self.env.get(&name).unwrap();
                Ok(value)
            }
            Expression::Assign(name, expr) => {
                let value = self.evaluate_expression(*expr)?;
                self.env.assign(name, value.clone());
                Ok(value)
            }
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
