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

mod environment;

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
    test_call_interpreter(input);

    // assert_eq!(result, Object::Number(18));
}

#[test]
fn test_evaluation_with_var_declaration() {
    let input = "var a = 10;\nvar b = 20;\nprint a * b;";
    test_call_interpreter(input);
}

#[test]
fn test_evaluation_with_var_assignment() {
    let input = "var a = 10;\nprint a = 25";
    test_call_interpreter(input);
}

#[test]
fn test_block_scope() {
    let input = String::from(
        r#"
        var a = "global a";
        var b = "global b";
        var c = "global c";
        {
            var a = "outer a";
            var b = "outer b";
            {
                var a = "inner a";
                print a;
                print b;
                print c;
            }
            print a;
            print b;
            print c;
        }
        print a;
        print b;
        print c;
        "#,
    );

    test_call_interpreter(input.as_str());
}

#[test]
fn test_if_statement() {
    let input = String::from(
        r#"
        if (1 > 3) {
            print "2 is greater than 1";
        } else {
            var a = 3;
            print a;
        }
        "#,
    );

    test_call_interpreter(input.as_str());
}

#[test]
fn test_if_logical_operator() {
    let input = String::from(
        r#"
        if (false and 9) {
            var a = 3 and 9;
            print a;
        } else {
            var b = 3 or 1;
            print b;
        }
        "#,
    );

    test_call_interpreter(input.as_str());
}

#[test]
fn test_while_statement() {
    let input = String::from(
        r#"
        var i = 0;
        while (i < 5) {
            i = i + 1;
            print i;
        };
        "#,
    );

    test_call_interpreter(input.as_str());
}

#[test]
fn test_for_statement() {
    let input = String::from(
        r#"
        for (var i = 0; i < 10; i = i + 1) {
            print i;
        };
        "#,
    );

    test_call_interpreter(input.as_str());
}

fn test_call_interpreter(input: &str) {
    let mut scanner = Scanner::new(input);
    let tokens = scanner.scan_tokens().unwrap();
    let mut iter = tokens.into_iter().peekable();
    let mut parser = Parser::new(&mut iter);
    let ast = parser.parse().unwrap();
    let mut interpreter = Interpreter::new();
    interpreter.evaluate(ast).unwrap();
}
