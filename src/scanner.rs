use std::{iter::Peekable, str::CharIndices};

use crate::token::Token;
pub struct Scanner {
    source: String,
}

impl Scanner {
    pub fn new(source: &str) -> Self {
        Scanner {
            source: source.to_string(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, ScannerError> {
        let mut tokens = vec![];
        for line in self.source.lines() {
            let line = line.trim();

            let mut char_indices = line.char_indices().peekable();

            while let Some((pos, ch)) = char_indices.next() {
                let token = match ch {
                    ' ' => continue,
                    '+' => Token::PLUS,
                    '-' => Token::MINUS,
                    '!' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => Token::BANG_EQUAL,
                        None => Token::BANG,
                    },
                    '>' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => Token::GREATER_EQUAL,
                        None => Token::GREATER,
                    },
                    '<' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => Token::LESS_EQUAL,
                        None => Token::LESS,
                    },
                    '=' => match char_indices.next_if_eq(&(pos + 1, '=')) {
                        Some(_equals) => Token::EQUAL_EQUAL,
                        None => Token::EQUAL,
                    },
                    '{' => Token::LEFT_BRACE,
                    '}' => Token::RIGHT_BRACE,
                    '(' => Token::LEFT_PAREN,
                    ')' => Token::RIGHT_PAREN,
                    '.' => Token::DOT,
                    ',' => Token::COMMA,
                    ';' => Token::SEMICOLON,
                    '"' => self.emit_string_token(&mut char_indices),
                    ch if ch.is_ascii_digit() => self.emit_number_token(ch, &mut char_indices),
                    ch if ch.is_ascii_alphabetic() || ch == '_' => {
                        self.emit_iden_token(ch, &mut char_indices)
                    }
                    '/' => match char_indices.next_if_eq(&(pos + 1, '/')) {
                        Some(_equals) => {
                            while let Some((_pos, _ch)) = char_indices.next() {
                                continue;
                            }
                            Token::INVALID("Comment".into())
                        }
                        None => Token::SLASH,
                    },
                    _ => Token::INVALID("Unrecognized character to tokenize".into()),
                };

                tokens.push(token);
            }
        }

        Ok(tokens)
    }

    fn match_reserved_word(&self, iden: &str) -> Option<Token> {
        match iden {
            "and" => Some(Token::AND),
            "class" => Some(Token::CLASS),
            "else" => Some(Token::ELSE),
            "false" => Some(Token::FALSE),
            "for" => Some(Token::FOR),
            "fun" => Some(Token::FUN),
            "if" => Some(Token::IF),
            "nil" => Some(Token::NIL),
            "or" => Some(Token::OR),
            "print" => Some(Token::PRINT),
            "return" => Some(Token::RETURN),
            "super" => Some(Token::SUPER),
            "this" => Some(Token::THIS),
            "true" => Some(Token::TRUE),
            "var" => Some(Token::VAR),
            "while" => Some(Token::WHILE),
            _ => None,
        }
    }

    fn emit_iden_token(&self, initial: char, char_indices: &mut Peekable<CharIndices>) -> Token {
        let mut iden = initial.to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| ch.is_alphabetic()) {
            iden.push(ch);
        }

        self.match_reserved_word(iden.as_str())
            .unwrap_or_else(|| Token::IDENTIFIER(iden))
    }

    fn emit_string_token(&self, char_indices: &mut Peekable<CharIndices>) -> Token {
        let mut val = "".to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| *ch != '"') {
            val.push(ch);
        }
        let maybe_quote = char_indices.next();
        match maybe_quote {
            Some(_) => Token::STRING(val),
            None => Token::INVALID("Unterminated string".into()),
        }
    }

    fn emit_number_token(&self, initial: char, char_indices: &mut Peekable<CharIndices>) -> Token {
        let mut digit = initial.to_string();
        while let Some((_pos, ch)) = char_indices.next_if(|(_pos, ch)| ch.is_ascii_digit()) {
            digit.push(ch);
        }

        let parsed: i64 = digit.parse().unwrap();
        Token::NUMBER(parsed)
    }
}

#[derive(Debug)]
pub enum ScannerError {
    ScanningError(String),
}
