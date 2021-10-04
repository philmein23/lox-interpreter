use std::collections::HashMap;
use std::convert::TryInto;
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

trait LoxCallable {
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Object, RuntimeError>;
}

#[derive(Clone, Debug)]
struct LoxFunction {
    name: String,
    params: Vec<String>,
    body: Vec<Statement>,
    closure: Environment,
}

impl LoxFunction {
    fn new(name: String, params: Vec<String>, body: Vec<Statement>, closure: Environment) -> Self {
        LoxFunction {
            name,
            params,
            body,
            closure,
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> u8 {
        self.params.len().try_into().unwrap()
    }

    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Object, RuntimeError> {
        let closure = self.closure.clone();
        let mut new_env = Environment::extend(closure);
        for (idx, param) in self.params.iter().enumerate() {
            new_env.define(param.into(), args[idx].clone());
        }
        // cache the global environment
        let saved_env = interpreter.env.clone();
        let saved_retval = interpreter.retval.clone();

        // set the env to newly created function's environment
        interpreter.env = new_env;

        let _ = interpreter.evaluate(self.body.clone());

        let retval = interpreter.retval.clone();
        // after evaluating function's body, then reset env with the global environment
        interpreter.env = saved_env;
        interpreter.retval = saved_retval;

        Ok(retval.unwrap_or_else(|| Object::Nil))
    }
}

#[derive(Clone, Debug)]
struct LoxClass {
    name: String,
}

impl LoxClass {
    fn new(name: String) -> Self {
        LoxClass { name }
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> u8 {
        0
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Object, RuntimeError> {
        let insta_id = interpreter.alloc_id();
        let instance = LoxInstance::new(self.name.clone());
        interpreter.lox_instances.insert(insta_id, instance);

        Ok(Object::LoxInstance(self.name.clone(), insta_id))
    }
}

#[derive(Clone, Debug)]
struct LoxInstance {
    class_name: String,
    fields: HashMap<String, Object>,
}

impl LoxInstance {
    fn new(class_name: String) -> Self {
        let fields = HashMap::new();
        LoxInstance { class_name, fields }
    }

    fn get(&self, prop: String) -> Option<Object> {
        if self.fields.contains_key(&prop) {
            let value = self.fields.get(&prop).map(|o| o.to_owned());
            value
        } else {
            None
        }
    }
}

pub struct Interpreter {
    env: Environment,
    lox_functions: HashMap<u64, LoxFunction>,
    lox_instances: HashMap<u64, LoxInstance>,
    lox_classes: HashMap<u64, LoxClass>,
    retval: Option<Object>,
    counter: u64,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let env = Environment::new();
        let lox_functions = HashMap::new();
        let lox_instances = HashMap::new();
        let lox_classes = HashMap::new();
        let retval = None;
        Interpreter {
            env,
            lox_functions,
            lox_instances,
            lox_classes,
            retval,
            counter: 0,
        }
    }

    fn alloc_id(&mut self) -> u64 {
        let id = self.counter;
        self.counter += 1;
        id
    }

    pub fn evaluate(&mut self, stmts: Vec<Statement>) -> Result<(), RuntimeError> {
        for stmt in stmts {
            // the first return statement should immediately follow an exit out of the function call
            if self.retval.is_some() {
                return Ok(());
            }
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
                Statement::While(cond, body) => {
                    self.evaluate_while_statement(&*cond, &*body);
                }
                Statement::Class(name, methods) => {
                    let class_id = self.alloc_id();
                    self.env
                        .define(name.clone(), Object::LoxClass(name.clone(), class_id));
                    let lox_class = LoxClass::new(name.clone());
                    self.lox_classes.insert(class_id, lox_class);
                }
                Statement::Function(name, params, body) => {
                    let func_id = self.alloc_id();
                    self.env
                        .define(name.clone(), Object::LoxFunction(name.clone(), func_id));
                    let lox_function =
                        LoxFunction::new(name.clone(), params, body, self.env.clone());
                    self.lox_functions.insert(func_id, lox_function);
                }
                Statement::Return(maybe_value) => {
                    self.retval = if *maybe_value != Expression::Nil {
                        Some(self.evaluate_expression(*maybe_value)?)
                    } else {
                        Some(Object::Nil)
                    };
                }
            }
        }

        Ok(())
    }

    fn is_truthy(val: &Object) -> bool {
        match val {
            Object::Nil => false,
            Object::Boolean(false) => false,
            Object::Boolean(true) => true,
            _ => true,
        }
    }

    fn evaluate_while_statement(&mut self, cond: &Expression, body: &Statement) {
        while Interpreter::is_truthy(&self.evaluate_expression(cond.to_owned()).unwrap()) {
            let mut statements = vec![];
            statements.push(body.to_owned());
            let _ = self.evaluate(statements);
        }
    }

    fn evaluate_if_statement(&mut self, cond: Expression, then: Statement, alt: Option<Statement>) {
        let evaluated = self.evaluate_expression(cond).unwrap();
        let mut statements = vec![];

        match Interpreter::is_truthy(&evaluated) {
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
        self.env = env;
        let _ = self.evaluate(statements);

        if let Some(enclosing) = self.env.enclosing.clone() {
            self.env = *enclosing;
        } else {
            panic!("Impossible");
        }
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
            Expression::Logical(left, logical_op, right) => {
                let left = self.evaluate_expression(*left)?;
                let right = self.evaluate_expression(*right)?;
                self.eval_infix_expression(logical_op, left, right)
            }
            Expression::Assign(name, expr) => {
                let value = self.evaluate_expression(*expr)?;
                self.env.assign(name, value.clone());
                Ok(value)
            }
            Expression::Call(callee, args) => {
                let callee = self.evaluate_expression(*callee)?;
                let eval_args = args
                    .iter()
                    .map(|arg| self.evaluate_expression(*arg.clone()).unwrap())
                    .collect::<Vec<Object>>();

                let mut value = Object::Nil;

                match callee {
                    Object::LoxFunction(name, func_id) => match self.lox_functions.get(&func_id) {
                        Some(f) => {
                            let f = f.clone();

                            value = f.call(self, &eval_args)?
                        }
                        None => panic!("There is no function with the name of {}", name),
                    },
                    Object::LoxClass(class_name, class_id) => match self.lox_classes.get(&class_id)
                    {
                        Some(class) => {
                            let class = class.clone();
                            value = class.call(self, &eval_args)?;
                        }
                        None => panic!("There is no class with the name of {}", class_name,),
                    },
                    _ => panic!("Callee with the name of {} does not exist", callee,),
                }

                Ok(value)
            }
            Expression::Get(object, property) => {
                // var obj = TestClass();
                // obj.getProp;
                let obj = self.evaluate_expression(*object)?;

                match obj {
                    Object::LoxInstance(_, id) => {
                        let instance = self.lox_instances.get(&id).map(|i| i.to_owned()).unwrap();
                        let value = instance.get(property).unwrap();
                        Ok(value)
                    }
                    _ => Err(RuntimeError::NewRuntimeError(format!(
                        "Object with property {} does not exist",
                        property
                    ))),
                }
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
            Infix::AND => {
                if !Interpreter::is_truthy(&left) {
                    return Ok(left);
                }

                Ok(right)
            }
            Infix::OR => {
                if Interpreter::is_truthy(&left) {
                    return Ok(left);
                }

                Ok(right)
            }
        }
    }
}
