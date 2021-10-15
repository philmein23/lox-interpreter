use std::cell::RefMut;
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
    func_id: u64,
    params: Vec<String>,
    body: Vec<Statement>,
    closure: Environment,
    is_initializer: bool,
}

impl LoxFunction {
    fn new(
        name: String,
        func_id: u64,
        params: Vec<String>,
        body: Vec<Statement>,
        closure: Environment,
        is_initializer: bool,
    ) -> Self {
        LoxFunction {
            name,
            func_id,
            params,
            body,
            closure,
            is_initializer,
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

        if self.is_initializer == true {
            let found_this = interpreter.env.get("this".into()).unwrap();
            return Ok(found_this);
        }
        // after evaluating function's body, then reset env with the global environment
        interpreter.env = saved_env;
        interpreter.retval = saved_retval;

        Ok(retval.unwrap_or_else(|| Object::Nil))
    }
}

#[derive(Clone, Debug)]
struct LoxClass {
    name: String,
    class_id: u64,
    super_class: Option<u64>,
    methods: HashMap<String, u64>,
}

impl LoxClass {
    fn new(name: String, class_id: u64, super_class: Option<u64>, methods: HashMap<String, u64>) -> Self {
        LoxClass {
            name,
            class_id,
            super_class,
            methods,
        }
    }
}

impl LoxClass {
    fn find_method(&self, interpreter: &Interpreter, method_name: &String) -> Option<u64> {
        match self.methods.get(method_name) {
            Some(method_id) => Some(*method_id),
            None => {
                if let Some(super_id) = self.super_class {
                    let found_class = interpreter.lox_classes.get(&super_id).unwrap();
                    let val = found_class.find_method(interpreter, method_name);
                    return val;
                } else {
                    return None;
                }
            }
        }
    }
}

impl LoxCallable for LoxClass {
    fn arity(&self) -> u8 {
        0
    }
    fn call(&self, interpreter: &mut Interpreter, args: &[Object]) -> Result<Object, RuntimeError> {
        let insta_id = interpreter.alloc_id();
        let instance = LoxInstance::new(self.name.clone(), insta_id);
        interpreter.lox_instances.insert(insta_id, instance);

        let instance_val = Object::LoxInstance(self.name.clone(), insta_id);

        // if there's a constructor (init) function, then execute it during the creation of an instance
        if let Some(id) = self.find_method(interpreter, &"init".into()) {
            let mut found_method = interpreter
                .lox_functions
                .get(&id)
                .map(|f| f.to_owned())
                .unwrap();
            let mut this_env = Environment::extend(found_method.closure.clone());
            this_env.define("this".into(), instance_val.clone());
            found_method.closure = this_env;

            interpreter.lox_functions.insert(id, found_method.clone());

            // calls the contructor with n arguments to initialize object with data
            let _ = found_method.call(interpreter, args);
        }

        Ok(instance_val)
    }
}

#[derive(Clone, Debug)]
struct LoxInstance {
    class_name: String,
    insta_id: u64,
    fields: HashMap<String, Object>,
}

impl LoxInstance {
    fn new(class_name: String, insta_id: u64) -> Self {
        let fields = HashMap::new();
        LoxInstance {
            class_name,
            insta_id,
            fields,
        }
    }

    fn get(&self, prop: String, interpreter: &mut Interpreter) -> Option<Object> {
        match self.fields.get(&prop) {
            Some(val) => Some(val.clone()),
            None => {
                let found_class = interpreter.env.get(&self.class_name);
                match found_class {
                    Some(Object::LoxClass(_, class_id)) => {
                        let class = interpreter.lox_classes.get(&class_id).unwrap();
                        let found_method_id = class.find_method(interpreter, &prop).unwrap();

                        if let Some(func) = interpreter.lox_functions.get_mut(&found_method_id) {
                            // ensure "this" is bound to the object that calls the method
                            let mut this_env = Environment::extend(func.closure.clone());
                            this_env.define(
                                "this".into(),
                                Object::LoxInstance(self.class_name.clone(), self.insta_id),
                            );
                            func.closure = this_env;

                            return Some(Object::LoxFunction(func.name.clone(), func.func_id));
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        }
    }
    fn set(&mut self, prop: String, value: Object) {
        self.fields.insert(prop, value);
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
                Statement::Class(name, super_class, methods) => {
                    let mut super_class_id = None;
                    if super_class.is_some() {
                        let sup = super_class.unwrap();
                        let evaled = self.evaluate_expression(*sup)?;

                        if let Object::LoxClass(_, id) = evaled {
                            super_class_id = Some(id);
                        }
                    }
                    let class_id = self.alloc_id();
                    self.env
                        .define(name.clone(), Object::LoxClass(name.clone(), class_id));
                    let mut methods_map: HashMap<String, u64> = HashMap::new();
                    for method in methods {
                        if let Statement::Function(name, params, body) = method {
                            let method_id = self.alloc_id();
                            let is_initializer = name.eq("init".into());
                            let lox_function = LoxFunction::new(
                                name.clone(),
                                method_id,
                                params,
                                body,
                                self.env.clone(),
                                is_initializer,
                            );
                            methods_map.insert(name.clone(), method_id);
                            self.lox_functions.insert(method_id, lox_function);
                        }
                    }
                    let lox_class =
                        LoxClass::new(name.clone(), class_id, super_class_id, methods_map);
                    self.lox_classes.insert(class_id, lox_class);
                }
                Statement::Function(name, params, body) => {
                    let func_id = self.alloc_id();
                    self.env
                        .define(name.clone(), Object::LoxFunction(name.clone(), func_id));
                    let lox_function = LoxFunction::new(
                        name.clone(),
                        func_id,
                        params,
                        body,
                        self.env.clone(),
                        false,
                    );
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
            Expression::This() => {
                let value = self.env.get("this".into()).unwrap();
                Ok(value)
            }
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
                // or TestClass().someProp;
                let obj = self.evaluate_expression(*object)?;

                match obj {
                    Object::LoxInstance(_, id) => {
                        let instance = self.lox_instances.get(&id).map(|i| i.to_owned()).unwrap();
                        let value = instance.get(property, self).unwrap();
                        Ok(value.clone())
                    }
                    _ => Err(RuntimeError::NewRuntimeError(format!(
                        "Object with property {} does not exist",
                        property
                    ))),
                }
            }
            Expression::Set(object, property, value) => {
                // obj.someProp = 13;
                let obj = self.evaluate_expression(*object)?;
                let value = self.evaluate_expression(*value)?;
                match obj {
                    Object::LoxInstance(_, id) => {
                        let instance = self.lox_instances.get_mut(&id).unwrap();
                        instance.set(property, value.clone());
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
