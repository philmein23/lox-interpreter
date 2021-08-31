use std::iter::Peekable;
use std::ptr::NonNull;
use std::vec::IntoIter;

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
            _ => {
                let stmt = self.statement()?;
                Ok(stmt)
            }
        }
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

    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.tokens.peek() {
            Some(Token::LEFT_BRACE) => self.block(),
            Some(Token::PRINT) => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.tokens.next(); // consume the ';'
        Ok(Statement::Expression(Box::new(expr)))
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
        let expr = self.equality();

        if let Some(Token::EQUAL) = self.tokens.peek() {
            self.tokens.next(); // consume the '='
            let value = self.assignment()?;

            if let Ok(Expression::Variable(name)) = expr {
                return Ok(Expression::Assign(name, Box::new(value)));
            } else {
                return Err(ParseError::NewParseError(
                    "Invalid assignment target".into(),
                ));
            }
        }

        expr
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

            _ => self.primary(),
        };
        result
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
