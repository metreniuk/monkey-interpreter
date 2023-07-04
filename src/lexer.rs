#![allow(dead_code)]
use std::{
    collections::HashMap,
    fmt::{self, Display},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(isize),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,
    Lte,
    Gte,
    Equals,
    NotEquals,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Ident(x) => write!(f, "{}", x),
            Token::Int(x) => write!(f, "{}", x),
            Token::Illegal => write!(f, "Illegal"),
            Token::Eof => write!(f, "Eof"),
            Token::Assign => write!(f, "="),
            Token::Bang => write!(f, "!"),
            Token::Minus => write!(f, "-"),
            Token::Slash => write!(f, "/"),
            Token::Asterisk => write!(f, "*"),
            Token::Equals => write!(f, "=="),
            Token::NotEquals => write!(f, "!="),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Lte => write!(f, "<="),
            Token::Gte => write!(f, ">="),
            Token::Plus => write!(f, "+"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
        }
    }
}

pub struct Lexer {
    input: String,
    postition: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            postition: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position);

        self.postition = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        self.input.chars().nth(self.read_position)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(ch) = self.ch {
                if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
                    self.read_char()
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let mut should_read_next = true;

        let token = match self.ch {
            None => Token::Eof,
            Some(c) => match c {
                '=' => self.read_binary('=', Token::Equals, Token::Assign),
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                ',' => Token::Comma,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' => self.read_binary('=', Token::NotEquals, Token::Bang),
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '<' => self.read_binary('=', Token::Lte, Token::Lt),
                '>' => self.read_binary('=', Token::Gte, Token::Gt),
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                ch => {
                    if is_letter(ch) {
                        let literal = self.read_identifier();
                        should_read_next = false;
                        self.lookup_keyword(literal)
                    } else if is_digit(ch) {
                        let num = self.read_number();
                        should_read_next = false;
                        Token::Int(num)
                    } else {
                        Token::Illegal
                    }
                }
            },
        };

        if should_read_next {
            self.read_char();
        }

        token
    }

    fn read_binary(&mut self, next: char, matched: Token, fallback: Token) -> Token {
        match self.peek_char() {
            Some(next_ch) => {
                if next_ch == next {
                    self.read_char();
                    matched
                } else {
                    fallback
                }
            }
            None => fallback,
        }
    }

    fn read_identifier(&mut self) -> String {
        let start_position = self.postition;

        while is_letter(self.ch.unwrap()) {
            self.read_char();
        }
        String::from(&self.input[start_position..self.postition])
    }

    fn read_number(&mut self) -> isize {
        let start_position = self.postition;

        while is_digit(self.ch.unwrap()) {
            self.read_char();
        }
        let str = &self.input[start_position..self.postition];

        str.parse().unwrap()
    }

    fn lookup_keyword(&self, str: String) -> Token {
        let mut keywords = HashMap::new();
        keywords.insert(String::from("let"), Token::Let);
        keywords.insert(String::from("fn"), Token::Function);
        keywords.insert(String::from("if"), Token::If);
        keywords.insert(String::from("else"), Token::Else);
        keywords.insert(String::from("return"), Token::Return);
        keywords.insert(String::from("true"), Token::True);
        keywords.insert(String::from("false"), Token::False);

        keywords.get(&str).unwrap_or(&Token::Ident(str)).clone()
    }

    fn eof(&self) -> bool {
        true
    }
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let input = String::from("=+(){},;");
        let mut output: Vec<Token> = vec![];

        let mut l = Lexer::new(input);

        loop {
            let token: Token = l.next_token();
            output.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(output, expected)
    }

    #[test]
    fn test_next_token() {
        let input = String::from(
            "
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;
",
        );
        let mut output: Vec<Token> = vec![];

        let mut l = Lexer::new(input);

        loop {
            let token: Token = l.next_token();
            output.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        let expected = vec![
            // let five = 5;
            // let ten = 10;
            // let add = fn(x, y) {
            //     x + y;
            // };
            // let result = add(five, ten);
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            // !-/*5;
            // 5 < 10 > 5;
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::Gt,
            Token::Int(5),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(output, expected);

        // for (i, x) in expected.iter().enumerate() {
        //     assert_eq!(output[i], *x)
        // }
    }

    #[test]
    fn test_next_token_more() {
        let input = String::from(
            "
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
",
        );
        let mut output: Vec<Token> = vec![];

        let mut l = Lexer::new(input);

        loop {
            let token: Token = l.next_token();
            output.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        let expected = vec![
            // if (5 < 10) {
            //     return true;
            // } else {
            //     return false;
            // }
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::Lt,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Eof,
        ];

        assert_eq!(output, expected);

        // for (i, x) in expected.iter().enumerate() {
        //     assert_eq!(output[i], *x)
        // }
    }

    #[test]
    fn test_next_token_binary() {
        let input = String::from(
            "
            10 == 10; 
            10 != 9;
            10 <= 9;
            10 >= 9;
            ",
        );
        let mut output: Vec<Token> = vec![];

        let mut l = Lexer::new(input);

        loop {
            let token: Token = l.next_token();
            output.push(token.clone());
            if token == Token::Eof {
                break;
            }
        }

        let expected = vec![
            Token::Int(10),
            Token::Equals,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::NotEquals,
            Token::Int(9),
            Token::Semicolon,
            Token::Int(10),
            Token::Lte,
            Token::Int(9),
            Token::Semicolon,
            Token::Int(10),
            Token::Gte,
            Token::Int(9),
            Token::Semicolon,
            Token::Eof,
        ];

        assert_eq!(output, expected);
    }
}
