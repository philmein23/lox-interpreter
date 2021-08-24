use std::iter::Peekable;
use std::vec::IntoIter;

use crate::ast::{Expression, Infix, Prefix};
use crate::token::Token;

pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    current: Option<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        let tokens = tokens.into_iter().peekable();
        Parser {
            tokens,
            current: None,
        }
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;
        loop {
            match self.tokens.peek() {
                Some(Token::BANG_EQUAL) => {
                    let op = Infix::BANG_EQUAL;
                    self.tokens.next();
                    let right = self.comparison()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::EQUAL_EQUAL) => {
                    let op = Infix::EQUAL_EQUAL;
                    self.tokens.next();
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
                    self.tokens.next();
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::GREATER_EQUAL) => {
                    let op = Infix::GREATER_EQUAL;
                    self.tokens.next();
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::LESS) => {
                    let op = Infix::LESS;
                    self.tokens.next();
                    let right = self.term()?;
                    expr = Expression::Binary(Box::new(expr), op, Box::new(right));
                }
                Some(Token::LESS_EQUAL) => {
                    let op = Infix::LESS_EQUAL;
                    self.tokens.next();
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
    }

    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;
    }

    fn unary(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.primary()?;
    }

    fn primary(&mut self) -> Result<Expression, ParseError> {}
}

pub enum ParseError {
    None,
    NewParseError(String),
}
