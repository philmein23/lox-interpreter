use core::fmt;
use std::{env::args, fs};

mod scanner;
use scanner::Scanner;

mod token;
use token::Token;

mod ast;

mod parser;
use parser::Parser;

mod interpreter;
use interpreter::Interpreter;

mod object;
use object::Object;

fn main() {
    for arg in args().skip(1) {
        let _result = run_file(arg).or_else(|e| Err(e));
    }
}

fn run_file(path: String) -> Result<(), Error> {
    let result = fs::read_to_string(path).ok();

    match result {
        Some(input) => {
            run(input);
            Ok(())
        }
        None => Err(Error::new("There was a problem reading path as string")),
    }
}

fn run(source: String) {
    let mut scanner = Scanner::new(&source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }
}

#[derive(Debug)]
struct Error {
    details: String,
}

impl Error {
    fn new(msg: &str) -> Error {
        Error {
            details: msg.to_string(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

#[test]
fn test_run_file() {
    let input = "var age = 12;\nage = 24;\n// This is a comment";
    // run(input.to_string());
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens().unwrap();
    let expected_tokens = vec![
        Token::VAR,
        Token::IDENTIFIER("age".to_string()),
        Token::EQUAL,
        Token::NUMBER(12),
        Token::SEMICOLON,
        Token::IDENTIFIER("age".to_string()),
        Token::EQUAL,
        Token::NUMBER(24),
        Token::SEMICOLON,
        Token::INVALID("Comment".into()),
    ];
    let mut iter = expected_tokens.into_iter();
    for token in tokens {
        assert_eq!(token, iter.next().unwrap());
    }
}

#[test]
fn test_evaluation() {
    let input = "print 1 + 2;";
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens().unwrap();
    let mut iter = tokens.into_iter().peekable();
    let mut parser = Parser::new(&mut iter);
    let ast = parser.parse().unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.evaluate(ast).unwrap();

    // assert_eq!(result, Object::Number(18));
}
