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

mod object;

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
    let input2 = "1 == 2 >= -6";
    // run(input.to_string());
    let mut scanner = Scanner::new(input);
    let mut scanner2 = Scanner::new(input2);
    let tokens = scanner.scan_tokens().unwrap();
    let tokens2 = scanner2.scan_tokens().unwrap();
    let mut iter2 = tokens2.into_iter().peekable();
    let mut parser = Parser::new(&mut iter2);
    let ast = parser.parse().unwrap();
    print!("AST: {:?}", ast);
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
