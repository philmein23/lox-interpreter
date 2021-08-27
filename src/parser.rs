use std::iter::Peekable;
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
            match self.tokens.peek() {
                None => break,
                Some(Token::PRINT) => {
                    let stmt = self.print_statement()?;
                    statements.push(stmt);
                }
                _ => {
                    let stmt = self.expression_statement()?;
                    statements.push(stmt);
                }
            }
        }
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.equality()
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.tokens.next(); // consume the ';'
        Ok(Statement::Expression(Box::new(expr)))
    }

    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        self.tokens.next(); // consume the print token
        let expr = self.expression()?;
        self.tokens.next(); // consume the ';'
        Ok(Statement::Print(Box::new(expr)))
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
