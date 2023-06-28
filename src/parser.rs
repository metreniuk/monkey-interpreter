#![allow(dead_code)]

use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq)]
struct Node {}

#[derive(Debug, PartialEq)]
enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
enum Expression {
    Empty,
    Ident(Token),
    IntegerLiteral(Token),
    BooleanLiteral(Token),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Empty => write!(f, "empty expression"),
            x => x.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
struct PrefixExpression {
    operator: Token,
    right: Expression,
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.operator {
            Token::Bang => write!(f, "(!{})", self.right),
            Token::Minus => write!(f, "(-{})", self.right),
            _ => write!(f, "unkown prefix expression"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct InfixExpression {
    operator: Token,
    left: Expression,
    right: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
struct LetStatement {
    name: Identifier,
    value: Expression,
}

#[derive(Debug, PartialEq)]
struct ReturnStatement {
    return_value: Expression,
}

#[derive(Debug, PartialEq)]
struct Identifier(Token);

#[derive(Debug, PartialEq)]
struct IntegerLiteral(Token);

#[derive(Debug, PartialEq)]
struct BooleanLiteral(Token);

type PrefixParse = fn() -> Expression;
type InfixParse = fn(Expression) -> Expression;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

struct Parser {
    l: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    precedence: HashMap<Token, Precedence>,
}

impl Parser {
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            curr_token: Token::Illegal,
            peek_token: Token::Illegal,
            errors: vec![],
            precedence: Parser::create_precedence(),
        };

        p.next_token();
        p.next_token();

        p
    }

    pub fn create_precedence() -> HashMap<Token, Precedence> {
        let mut hm = HashMap::new();
        hm.insert(Token::Equals, Precedence::Equals);
        hm.insert(Token::NotEquals, Precedence::Equals);
        hm.insert(Token::Lt, Precedence::LessGreater);
        hm.insert(Token::Gt, Precedence::LessGreater);
        hm.insert(Token::Plus, Precedence::Sum);
        hm.insert(Token::Minus, Precedence::Sum);
        hm.insert(Token::Slash, Precedence::Product);
        hm.insert(Token::Asterisk, Precedence::Product);

        hm
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while !self.curr_token_is(Token::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }

            self.next_token();
        }

        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_ident_peek() {
            return None;
        }

        let name = Identifier(self.curr_token.clone());

        if !self.expect_peek(Token::Assign) {
            return None;
        }

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Let(LetStatement {
            name,
            value: Expression::Empty,
        }));
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        while !self.curr_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Return(ReturnStatement {
            return_value: Expression::Empty,
        }));
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if !self.expect_peek(Token::Semicolon) {
            return None;
        }

        Some(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        // println!("parse_expression START");
        let r = match self.parse_prefix_expression() {
            Err(token) => {
                self.no_prefix_parse_error(token);
                None
            }
            Ok(mut left) => {
                while !self.peek_token_is(Token::Semicolon)
                    && precedence < self.peek_precedence()
                    && self.has_peek_infix_parse()
                {
                    self.next_token();
                    left = Expression::Infix(self.parse_infix_expression(left).unwrap());
                }
                Some(left)
            }
        };
        // println!("parse_expression: {:?}", r);
        r
    }

    fn parse_prefix_inner(&mut self) -> Result<Box<PrefixExpression>, Token> {
        let t = self.curr_token.clone();

        self.next_token();

        match self.parse_expression(Precedence::Prefix) {
            Some(right) => Ok(Box::new(PrefixExpression { operator: t, right })),
            None => Err(t),
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Box<InfixExpression>, Token> {
        // println!("parse_infix_expression START");
        let precedence = self.curr_precedence();
        let t = self.curr_token.clone();

        self.next_token();

        let r = match self.parse_expression(precedence) {
            Some(right) => Ok(Box::new(InfixExpression {
                operator: t,
                left,
                right,
            })),
            None => Err(t),
        };
        // println!("parse_infix_expression: {:?}", r);
        r
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, Token> {
        // println!("parse_prefix_expression START");
        let r = match self.curr_token.clone() {
            Token::Bang | Token::Minus => self
                .parse_prefix_inner()
                .map(|prefix_exp| Expression::Prefix(prefix_exp)),
            Token::Ident(_) => Ok(self.parse_identifier()),
            Token::Int(_) => Ok(self.parse_integer_literal()),
            Token::LParen => self.parse_grouped_expression(),
            Token::True | Token::False => Ok(self.parse_boolean_literal()),
            t => Err(t),
        };
        // println!("parse_prefix_expression: {:?}", r);
        r
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Token> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::RParen) {
            return Err(self.peek_token.clone());
        }
        return exp.map_or(Err(self.curr_token.clone()), |ex| Ok(ex));
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Ident(self.curr_token.clone())
    }

    fn parse_integer_literal(&self) -> Expression {
        Expression::IntegerLiteral(self.curr_token.clone())
    }

    fn parse_boolean_literal(&self) -> Expression {
        Expression::BooleanLiteral(self.curr_token.clone())
    }

    fn has_peek_infix_parse(&mut self) -> bool {
        match self.peek_token.clone() {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equals
            | Token::NotEquals
            | Token::Lt
            | Token::Gt => true,
            _ => false,
        }
    }

    fn no_prefix_parse_error(&mut self, token: Token) {
        self.errors
            .push(format!("no prefix parse fn for {token} found"))
    }

    fn expect_ident_peek(&mut self) -> bool {
        match self.peek_token {
            Token::Ident(_) => {
                self.next_token();
                true
            }
            _ => false,
        }
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn peek_error(&mut self, token: Token) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            token, self.peek_token
        );
        self.errors.push(msg);
    }

    fn assert_peek(&mut self, token: Token) {
        assert_eq!(self.peek_token, token);
        self.next_token();
    }

    fn curr_token_is(&self, token: Token) -> bool {
        self.curr_token == token
    }

    fn peek_token_is(&self, token: Token) -> bool {
        self.peek_token == token
    }

    fn peek_precedence(&self) -> Precedence {
        self.precedence
            .get(&self.peek_token)
            .unwrap_or(&Precedence::Lowest)
            .clone()
    }

    fn curr_precedence(&self) -> Precedence {
        self.precedence
            .get(&self.curr_token)
            .unwrap_or(&Precedence::Lowest)
            .clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    #[test]
    fn test_let() {
        let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert!(program.statements.len() == 3);
        assert_eq!(
            program.statements[0],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("x"))),
                value: Expression::Empty
            })
        );
        assert_eq!(
            program.statements[1],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("y"))),
                value: Expression::Empty
            })
        );
        assert_eq!(
            program.statements[2],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("foobar"))),
                value: Expression::Empty
            })
        );
    }

    #[test]
    fn test_return() {
        let input = "
        return 5;
        return 10;
        return 993322;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert!(program.statements.len() == 3);
        assert_eq!(
            program.statements[0],
            Statement::Return(ReturnStatement {
                return_value: Expression::Empty
            })
        );
        assert_eq!(
            program.statements[0],
            Statement::Return(ReturnStatement {
                return_value: Expression::Empty
            })
        );
        assert_eq!(
            program.statements[0],
            Statement::Return(ReturnStatement {
                return_value: Expression::Empty
            })
        );
    }

    #[test]
    fn test_expr_ident() {
        let input = "
        foobar;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Ident(Token::Ident(String::from("foobar"))))
        );
    }

    #[test]
    fn test_expr_integer() {
        let input = "
        5;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::IntegerLiteral(Token::Int(5)))
        );
    }

    #[test]
    fn test_expr_boolean() {
        let input = "
        true;
        false;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 2);
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::BooleanLiteral(Token::True))
        );
        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::BooleanLiteral(Token::False))
        );
    }

    #[test]
    fn test_expr_prefix() {
        let input = "
        !5;
        -15;
        !true;
        !false;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 4);
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Prefix(Box::new(PrefixExpression {
                operator: Token::Bang,
                right: Expression::IntegerLiteral(Token::Int(5))
            })))
        );
        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::Prefix(Box::new(PrefixExpression {
                operator: Token::Minus,
                right: Expression::IntegerLiteral(Token::Int(15))
            })))
        );
        assert_eq!(
            program.statements[2],
            Statement::Expression(Expression::Prefix(Box::new(PrefixExpression {
                operator: Token::Bang,
                right: Expression::BooleanLiteral(Token::True)
            })))
        );
        assert_eq!(
            program.statements[3],
            Statement::Expression(Expression::Prefix(Box::new(PrefixExpression {
                operator: Token::Bang,
                right: Expression::BooleanLiteral(Token::False)
            })))
        );
    }

    #[test]
    fn test_expr_infix() {
        let input = "
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 8);

        let expected_tokens = vec![
            Token::Plus,
            Token::Minus,
            Token::Asterisk,
            Token::Slash,
            Token::Gt,
            Token::Lt,
            Token::Equals,
            Token::NotEquals,
        ];

        for (i, t) in expected_tokens.iter().enumerate() {
            assert_eq!(
                program.statements[i],
                Statement::Expression(Expression::Infix(Box::new(InfixExpression {
                    operator: t.clone(),
                    left: Expression::IntegerLiteral(Token::Int(5)),
                    right: Expression::IntegerLiteral(Token::Int(5))
                })))
            )
        }
    }

    #[test]
    fn test_expr_infix_long() {
        let input = "
        -1 < 2 - 3 * 4 > 5;
        3 > 5 == false;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 2);

        // (1 < (2 - (3 * 4))) > 5;
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Infix(Box::new(InfixExpression {
                operator: Token::Gt,
                left: Expression::Infix(Box::new(InfixExpression {
                    operator: Token::Lt,
                    left: Expression::Prefix(Box::new(PrefixExpression {
                        operator: Token::Minus,
                        right: Expression::IntegerLiteral(Token::Int(1)),
                    })),
                    right: Expression::Infix(Box::new(InfixExpression {
                        operator: Token::Minus,
                        left: Expression::IntegerLiteral(Token::Int(2)),
                        right: Expression::Infix(Box::new(InfixExpression {
                            operator: Token::Asterisk,
                            left: Expression::IntegerLiteral(Token::Int(3)),
                            right: Expression::IntegerLiteral(Token::Int(4)),
                        })),
                    })),
                })),
                right: Expression::IntegerLiteral(Token::Int(5)),
            })))
        );
        // (3 > 5) == false;
        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::Infix(Box::new(InfixExpression {
                operator: Token::Equals,
                left: Expression::Infix(Box::new(InfixExpression {
                    operator: Token::Gt,
                    left: Expression::IntegerLiteral(Token::Int(3)),
                    right: Expression::IntegerLiteral(Token::Int(5))
                })),
                right: Expression::BooleanLiteral(Token::False),
            })))
        )
    }

    #[test]
    fn test_expr_grouped() {
        let input = "
        (1) + (2 - 3);
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {:?}", p.errors);
        }

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Infix(Box::new(InfixExpression {
                operator: Token::Plus,
                left: Expression::IntegerLiteral(Token::Int(1)),
                right: Expression::Infix(Box::new(InfixExpression {
                    operator: Token::Minus,
                    left: Expression::IntegerLiteral(Token::Int(2)),
                    right: Expression::IntegerLiteral(Token::Int(3))
                })),
            })))
        );
    }

    // #[test]
    // fn test_expr_precedence() {
    //     let inputs = vec![
    //         "-a * b;
    //         ((-a) * b);",
    //         "!-a;
    //         (!(-a));",
    //         "a + b + c;
    //         ((a + b) + c);",
    //         "a + b - c;
    //         ((a + b) - c);",
    //         "a * b * c;
    //         ((a * b) * c);",
    //         "a * b / c;
    //         ((a * b) / c);",
    //         "a + b / c;
    //         (a + (b / c));",
    //         "a + b * c + d / e - f;
    //         (((a + (b * c)) + (d / e)) - f);",
    //         "3 + 4; -5 * 5;
    //         (3 + 4)((-5) * 5);",
    //         "5 > 4 == 3 < 4;
    //         ((5 > 4) == (3 < 4));",
    //         "5 < 4 != 3 > 4;
    //         ((5 < 4) != (3 > 4));",
    //         "3 + 4 * 5 == 3 * 1 + 4 * 5;
    //         ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
    //         "3 + 4 * 5 == 3 * 1 + 4 * 5;
    //         ((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
    //     ];

    //     for input in inputs.iter() {
    //         let l = Lexer::new(String::from(*input));
    //         let mut p = Parser::new(l);
    //         let program = p.parse_program();

    //         if !p.errors.is_empty() {
    //             panic!("Parsing errors: {:?}", p.errors);
    //         }

    //         assert_eq!(program.statements.len(), 2);

    //         assert_eq!(program.statements[0], program.statements[1],)
    //     }
    // }
}
