use std::iter::Peekable;
use std::vec::{self, IntoIter};

use crate::ast::{Expression, Infix, Prefix, Statement};
use crate::token::Token;

pub struct Parser<'a> {
    tokens: &'a mut Peekable<IntoIter<Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a mut Peekable<IntoIter<Token>>) -> Self {
        Parser { tokens }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = vec![];
        loop {
            let stmt = self.declaration();
            match stmt {
                Ok(stmt) => statements.push(stmt),
                Err(_) => break,
            }
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.peek() {
            None => Err(ParseError::None),
            Some(Token::VAR) => {
                let stmt = self.var_declaration()?;
                Ok(stmt)
            }
            Some(Token::FUN) => {
                self.tokens.next(); // consume the 'fun' token
                let stmt = self.function()?;
                Ok(stmt)
            }
            Some(Token::CLASS) => {
                let stmt = self.class_declaration()?;
                Ok(stmt)
            }
            _ => {
                let stmt = self.statement()?;
                Ok(stmt)
            }
        }
    }

    fn class_declaration(&mut self) -> Result<Statement, ParseError> {
        let name = match self.tokens.peek() {
            Some(Token::IDENTIFIER(name)) => name.to_string(),
            _ => {
                return Err(ParseError::NewParseError(
                    "Expected function identifier".into(),
                ))
            }
        };
        self.tokens.next(); // consume the identifier token
        self.tokens.next(); // consume the '{' token

        let mut methods = vec![];

        loop {
            if let Some(token) = self.tokens.peek() {
                if *token != Token::RIGHT_BRACE {
                    let method = self.function()?;
                    methods.push(method);
                } else {
                    break;
                }
            }
        }

        self.tokens.next(); // consume the '}' token

        Ok(Statement::Class(name, methods))
    }

    fn var_declaration(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the var token
        let name = match self.tokens.peek() {
            Some(Token::IDENTIFIER(name)) => name.to_string(),
            _ => return Err(ParseError::NewParseError("Expected identifier".into())),
        };
        self.tokens.next(); // consume the identifier

        let init = if let Some(Token::EQUAL) = self.tokens.peek() {
            self.tokens.next(); // consume the equals
            Some(self.expression()?)
        } else {
            None
        };

        self.tokens.next(); // consume the semicolon

        match init {
            Some(initiator) => Ok(Statement::Var(name, Some(Box::new(initiator)))),
            None => Ok(Statement::Var(name, None)),
        }
    }

    fn function(&mut self) -> Result<Statement, ParseError> {
        let name = match self.tokens.peek() {
            Some(Token::IDENTIFIER(name)) => name.to_string(),
            _ => {
                return Err(ParseError::NewParseError(
                    "Expected function identifier".into(),
                ))
            }
        };
        self.tokens.next(); // consume the identifier
        self.tokens.next(); // consume the left paren

        let mut params = vec![];

        if let Some(token) = self.tokens.peek() {
            if *token != Token::RIGHT_PAREN {
                if let Some(Token::IDENTIFIER(param)) = self.tokens.peek() {
                    params.push(param.to_string());
                    self.tokens.next(); // consume the identifier
                }

                while let Some(Token::COMMA) = self.tokens.peek() {
                    self.tokens.next(); // consume the ','

                    if let Some(Token::IDENTIFIER(param)) = self.tokens.peek() {
                        params.push(param.to_string());
                        self.tokens.next(); // consume the identifier
                    }
                }
            }
        }

        self.tokens.next(); // consume the ')'

        let body;
        match self.block()? {
            Statement::Block(stmts) => {
                body = stmts;
            }
            _ => panic!("Expected block statement".to_string()),
        };

        Ok(Statement::Function(name, params, body))
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.peek() {
            Some(Token::IF) => self.if_statement(),
            Some(Token::LEFT_BRACE) => self.block(),
            Some(Token::PRINT) => self.print_statement(),
            Some(Token::WHILE) => self.while_statement(),
            Some(Token::FOR) => self.for_statement(),
            Some(Token::RETURN) => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the 'return'
        let mut value = Expression::Nil;
        if let Some(Token::SEMICOLON) = self.tokens.peek() {
            self.tokens.next(); // consume the ; token
        } else {
            value = self.expression()?;
            self.tokens.next(); // consume the ; token
        }

        Ok(Statement::Return(Box::new(value)))
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the 'for'
        self.tokens.next(); // consume the '('

        let initializer = match self.tokens.peek() {
            Some(Token::SEMICOLON) => {
                self.tokens.next(); // consume the ';'
                None
            }
            Some(Token::VAR) => self.var_declaration().ok(),
            _ => self.expression_statement().ok(),
        };

        let condition = match self.tokens.peek() {
            Some(Token::SEMICOLON) => {
                self.tokens.next(); // consume the ';'
                None
            }
            _ => {
                let expr = self.expression().ok();
                self.tokens.next(); // consume the ';'
                expr
            }
        };

        let increment = match self.tokens.peek() {
            Some(Token::RIGHT_PAREN) => {
                self.tokens.next(); // consume the ')'
                None
            }
            _ => {
                let expr = self.expression().ok();
                self.tokens.next(); // consume the ')'
                expr
            }
        };

        let mut body = self.statement();

        if let Some(expr) = increment {
            body = Ok(Statement::Block(vec![
                body?,
                Statement::Expression(Box::new(expr)),
            ]));
        }

        body = if let Some(cond) = condition {
            Ok(Statement::While(Box::new(cond), Box::new(body?)))
        } else {
            Ok(Statement::While(
                Box::new(Expression::Boolean(true)),
                Box::new(body?),
            ))
        };

        if let Some(init) = initializer {
            body = Ok(Statement::Block(vec![init, body?]))
        }

        body
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.tokens.next(); // consume the ';'
        Ok(Statement::Expression(Box::new(expr)))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the 'while'
        self.tokens.next(); // consume the '('

        let condition = self.expression()?;

        self.tokens.next(); // consume the ')';

        let body = self.statement()?;

        Ok(Statement::While(Box::new(condition), Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the 'if'
        self.tokens.next(); // comsume the '('

        let condition = self.expression()?;

        self.tokens.next(); // consume the ')'
        let then_branch = self.statement()?;

        match self.tokens.peek() {
            Some(Token::ELSE) => {
                self.tokens.next(); // consume the 'else'
                let else_branch = self.statement()?;
                Ok(Statement::If(
                    Box::new(condition),
                    Box::new(then_branch),
                    Some(Box::new(else_branch)),
                ))
            }
            _ => Ok(Statement::If(
                Box::new(condition),
                Box::new(then_branch),
                None,
            )),
        }
    }

    fn block(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the '{'
        let mut statements = vec![];
        loop {
            if Some(&Token::RIGHT_BRACE) == self.tokens.peek() {
                self.tokens.next(); // consume the '}'
                break;
            }
            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        Ok(Statement::Block(statements))
    }

    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the print token
        let expr = self.expression()?;
        self.tokens.next(); // consume the ';'
        Ok(Statement::Print(Box::new(expr)))
    }

    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.or();

        if let Some(Token::EQUAL) = self.tokens.peek() {
            self.tokens.next(); // consume the '='
            let value = self.assignment()?;
            let expr = expr.clone();
            match expr {
                Ok(Expression::Variable(name)) => Ok(Expression::Assign(name, Box::new(value))),
                Ok(Expression::Get(obj, prop)) => Ok(Expression::Set(obj, prop, Box::new(value))),
                _ => Err(ParseError::NewParseError(
                    "Invalid assignment target".into(),
                )),
            };
        }

        expr
    }

    fn or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.and()?;

        loop {
            if let Some(Token::OR) = self.tokens.peek() {
                self.tokens.next(); // consume the 'or'
                let op = Infix::OR;
                let right = self.and()?;
                expr = Expression::Logical(Box::new(expr), op, Box::new(right))
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.equality()?;

        loop {
            if let Some(Token::AND) = self.tokens.peek() {
                self.tokens.next(); // consume the 'or'
                let op = Infix::AND;
                let right = self.equality()?;
                expr = Expression::Logical(Box::new(expr), op, Box::new(right))
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;
        loop {
            match self.tokens.peek() {
                Some(Token::BANG_EQUAL) => {
                    let op = Infix::BANG_EQUAL;
                    self.tokens.next(); // consume the '!='
                    let right = self.comparison()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::EQUAL_EQUAL) => {
                    let op = Infix::EQUAL_EQUAL;
                    self.tokens.next(); // consume the '=='
                    let right = self.comparison()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.term()?;
        loop {
            match self.tokens.peek() {
                Some(Token::GREATER) => {
                    let op = Infix::GREATER;
                    self.tokens.next(); // consume the '>'
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::GREATER_EQUAL) => {
                    let op = Infix::GREATER_EQUAL;
                    self.tokens.next(); // consume the '>='
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::LESS) => {
                    let op = Infix::LESS;
                    self.tokens.next(); // consume the '<'
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::LESS_EQUAL) => {
                    let op = Infix::LESS_EQUAL;
                    self.tokens.next(); // consume the '<='
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.factor()?;
        loop {
            match self.tokens.peek() {
                Some(Token::MINUS) => {
                    let op = Infix::MINUS;
                    self.tokens.next(); // consume the '-'
                    let right = self.factor()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::PLUS) => {
                    let op = Infix::PLUS;
                    self.tokens.next(); // consume the '+'
                    let right = self.factor()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;
        loop {
            match self.tokens.peek() {
                Some(Token::SLASH) => {
                    let op = Infix::SLASH;
                    self.tokens.next(); // consume the '/'
                    let right = self.unary()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::STAR) => {
                    let op = Infix::STAR;
                    self.tokens.next(); // consume the '*'
                    let right = self.unary()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParseError> {
        let result = match self.tokens.peek() {
            Some(Token::BANG) => {
                self.tokens.next(); // consume the '!'
                let right = self.unary()?;
                Ok(Expression::Unary(Prefix::BANG, Box::new(right)))
            }
            Some(Token::MINUS) => {
                self.tokens.next(); // consume the '-'
                let right = self.unary()?;
                Ok(Expression::Unary(Prefix::MINUS, Box::new(right)))
            }

            _ => self.call(),
        };
        result
    }

    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.primary()?;

        loop {
            match self.tokens.peek() {
                Some(Token::LEFT_PAREN) => {
                    self.tokens.next(); // consume the '(' token
                    expr = self.finish_call(expr)?;
                }
                Some(Token::DOT) => {
                    // someObject.age;
                    self.tokens.next(); // consume the '.' token
                    if let Some(Token::IDENTIFIER(iden)) = self.tokens.peek() {
                        expr = Expression::Get(Box::new(expr), iden.to_owned());
                    }
                    self.tokens.next(); // consume the 'iden' token
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, ParseError> {
        let mut args = vec![];

        if let Some(token) = self.tokens.peek() {
            if *token != Token::RIGHT_PAREN {
                let boxed = Box::new(self.expression()?);
                args.push(boxed);

                while let Some(Token::COMMA) = self.tokens.peek() {
                    self.tokens.next(); // consume the ','

                    let boxed = Box::new(self.expression()?);
                    args.push(boxed);
                }
            }
        }
        self.tokens.next(); // consume the ')'

        Ok(Expression::Call(Box::new(callee), args))
    }

    fn primary(&mut self) -> Result<Expression, ParseError> {
        let result = match self.tokens.peek() {
            Some(Token::FALSE) => Ok(Expression::Boolean(false)),
            Some(Token::TRUE) => Ok(Expression::Boolean(true)),
            Some(Token::NIL) => Ok(Expression::Nil),
            Some(Token::NUMBER(n)) => Ok(Expression::Number(*n)),
            Some(Token::STRING(s)) => Ok(Expression::StringLiteral(s.to_string())),
            Some(Token::LEFT_PAREN) => {
                self.tokens.next(); // consume the '('
                let expr = self.expression()?;
                let maybe_right_paren = self.tokens.next(); // consume the ')'

                if maybe_right_paren != Some(Token::RIGHT_PAREN) {
                    return Err(ParseError::NewParseError("Expected ')' after.".into()));
                }

                Ok(Expression::Grouping(Box::new(expr)))
            }
            Some(Token::IDENTIFIER(s)) => Ok(Expression::Variable(s.to_string())),
            _ => Err(ParseError::NewParseError("Expected expression.".into())),
        };
        self.tokens.next(); // consume the token

        result
    }

    fn synchronize(&mut self) {
        let mut maybe_semicolon = self.tokens.next(); // consume the ';'
        loop {
            if maybe_semicolon == Some(Token::SEMICOLON) {
                return;
            }

            match self.tokens.peek() {
                Some(Token::CLASS) | Some(Token::FUN) | Some(Token::VAR) | Some(Token::FOR)
                | Some(Token::IF) | Some(Token::WHILE) | Some(Token::PRINT)
                | Some(Token::RETURN) => {
                    return;
                }
                _ => {
                    maybe_semicolon = self.tokens.next();
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    None,
    NewParseError(String),
}
