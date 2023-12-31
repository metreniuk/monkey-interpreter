#![allow(dead_code)]

use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::{Lexer, Token};

pub struct Parser {
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

    fn create_precedence() -> HashMap<Token, Precedence> {
        let mut hm = HashMap::new();
        hm.insert(Token::Equals, Precedence::Equals);
        hm.insert(Token::NotEquals, Precedence::Equals);
        hm.insert(Token::Lt, Precedence::LessGreater);
        hm.insert(Token::Gt, Precedence::LessGreater);
        hm.insert(Token::Plus, Precedence::Sum);
        hm.insert(Token::Minus, Precedence::Sum);
        hm.insert(Token::Slash, Precedence::Product);
        hm.insert(Token::Asterisk, Precedence::Product);
        hm.insert(Token::LParen, Precedence::Call);
        hm.insert(Token::LBracket, Precedence::PropertyAccess);

        hm
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
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

        self.expect_peek_(Token::Assign).ok().unwrap();

        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Let(LetStatement { name, value: exp }));
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        return Some(Statement::Return(ReturnStatement { return_value: exp }));
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let mut stmts = vec![];

        self.next_token();

        while !self.curr_token_is(Token::RBrace) && !self.curr_token_is(Token::Eof) {
            stmts.push(self.parse_statement()?);
            self.next_token();
        }

        Some(BlockStatement { statements: stmts })
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token_is(Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Expression(expr))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        match self.parse_prefix_expression() {
            Err(token) => {
                self.no_prefix_parse_error(token);
                None
            }
            Ok(mut left) => {
                while !self.peek_token_is(Token::Semicolon) && precedence < self.peek_precedence() {
                    let t = self.has_peek_infix_parse()?;

                    self.next_token();
                    let l = left.clone();

                    left = match self.parse_infix_expression(left, t) {
                        Ok(x) => x,
                        Err(_) => return Some(l),
                    }
                }
                Some(left)
            }
        }
    }

    fn parse_prefix_inner(&mut self) -> Result<Box<PrefixExpression>, Token> {
        let t = self.curr_token.clone();

        self.next_token();

        self.parse_expression(Precedence::Prefix)
            .map(|right| {
                Box::new(PrefixExpression {
                    operator: t.clone(),
                    right,
                })
            })
            .ok_or(t)
    }

    fn parse_infix_expression(
        &mut self,
        left: Expression,
        match_token: Token,
    ) -> Result<Expression, Token> {
        match match_token {
            // call expression
            Token::LParen => self.parse_call_arguments().map(|arguments| {
                Expression::Call(Box::new(CallExpression {
                    function: left,
                    arguments,
                }))
            }),
            Token::LBracket => self.parse_property_access().map(|property| {
                Expression::PropertyAccess(Box::new(PropertyAccess {
                    object: left,
                    property,
                }))
            }),
            // infix operator
            _ => {
                let precedence = self.curr_precedence();
                let t = self.curr_token.clone();

                self.next_token();

                self.parse_expression(precedence)
                    .map(|right| {
                        Expression::Operation(Box::new(InfixExpression {
                            operator: t.clone(),
                            left,
                            right,
                        }))
                    })
                    .ok_or(t)
            }
        }
    }

    fn has_peek_infix_parse(&mut self) -> Option<Token> {
        match self.peek_token.clone() {
            Token::Plus
            | Token::Minus
            | Token::Slash
            | Token::Asterisk
            | Token::Equals
            | Token::NotEquals
            | Token::LParen
            | Token::LBracket
            | Token::Lt
            | Token::Gt => Some(self.peek_token.clone()),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, Token> {
        match self.curr_token.clone() {
            Token::Bang | Token::Minus => self
                .parse_prefix_inner()
                .map(|prefix_exp| Expression::Prefix(prefix_exp)),
            Token::Int(_) => Ok(self.parse_integer_literal()),
            Token::String(_) => Ok(self.parse_string_literal()),
            Token::LParen => self.parse_grouped_expression(),
            Token::LBracket => self.parse_array_expression(),
            Token::Ident(_) => Ok(self.parse_identifier()),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            Token::True | Token::False => Ok(self.parse_boolean_literal()),
            t => Err(t),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, Token> {
        if !self.expect_peek(Token::LParen) {
            return Err(self.peek_token.clone());
        }

        self.next_token(); // pass the if

        let condition = self
            .parse_expression(Precedence::Lowest)
            .ok_or(self.curr_token.clone())?;

        if !self.expect_peek(Token::RParen) {
            return Err(self.peek_token.clone());
        }

        if !self.expect_peek(Token::LBrace) {
            return Err(self.peek_token.clone());
        }

        let consequence = self
            .parse_block_statement()
            .ok_or(self.curr_token.clone())?;

        let alternative = if self.peek_token_is(Token::Else) {
            self.next_token();

            if !self.expect_peek(Token::LBrace) {
                return Err(self.curr_token.clone());
            }

            self.parse_block_statement()
                .ok_or(self.curr_token.clone())?
        } else {
            BlockStatement::default()
        };

        Ok(Expression::If(Box::new(IfExpression {
            condition,
            consequence,
            alternative,
        })))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Token> {
        self.next_token();

        let exp = self.parse_expression(Precedence::Lowest);

        if !self.expect_peek(Token::RParen) {
            return Err(self.peek_token.clone());
        }
        return exp.map_or(Err(self.curr_token.clone()), |ex| Ok(ex));
    }

    fn parse_function_literal(&mut self) -> Result<Expression, Token> {
        self.expect_peek_(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek_(Token::LBrace)?;

        let body = self
            .parse_block_statement()
            .ok_or(self.curr_token.clone())?;

        Ok(Expression::Fn(FunctionLiteral { parameters, body }))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>, Token> {
        let mut params = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(params);
        }

        self.next_token();

        params.push(Identifier(self.curr_token.clone()));

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();
            params.push(Identifier(self.curr_token.clone()));
        }

        self.expect_peek_(Token::RParen)?;

        Ok(params)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, Token> {
        let mut args = vec![];

        if self.peek_token_is(Token::RParen) {
            self.next_token();
            return Ok(args);
        }

        self.next_token();

        args.push(
            self.parse_expression(Precedence::Lowest)
                .ok_or(self.curr_token.clone())?,
        );

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            args.push(
                self.parse_expression(Precedence::Lowest)
                    .ok_or(self.curr_token.clone())?,
            );
        }

        self.expect_peek_(Token::RParen)?;

        Ok(args)
    }

    fn parse_array_expression(&mut self) -> Result<Expression, Token> {
        let elements = self.parse_array_elements()?;

        Ok(Expression::Array(Box::new(Array { elements })))
    }

    fn parse_array_elements(&mut self) -> Result<Vec<Expression>, Token> {
        let mut elements = vec![];

        if self.peek_token_is(Token::RBracket) {
            self.next_token();
            return Ok(elements);
        }

        self.next_token();

        elements.push(
            self.parse_expression(Precedence::Lowest)
                .ok_or(self.curr_token.clone())?,
        );

        while self.peek_token_is(Token::Comma) {
            self.next_token();
            self.next_token();

            elements.push(
                self.parse_expression(Precedence::Lowest)
                    .ok_or(self.curr_token.clone())?,
            );
        }

        self.expect_peek_(Token::RBracket)?;

        Ok(elements)
    }

    fn parse_property_access(&mut self) -> Result<Expression, Token> {
        self.next_token();

        let expr = self
            .parse_expression(Precedence::Lowest)
            .ok_or(self.curr_token.clone())?;

        self.expect_peek_(Token::RBracket)?;
        Ok(expr)
    }

    fn parse_identifier(&self) -> Expression {
        Expression::Ident(self.curr_token.clone())
    }

    fn parse_integer_literal(&self) -> Expression {
        Expression::IntegerLiteral(self.curr_token.clone())
    }

    fn parse_string_literal(&self) -> Expression {
        Expression::StringLiteral(self.curr_token.clone())
    }

    fn parse_boolean_literal(&self) -> Expression {
        Expression::BooleanLiteral(self.curr_token.clone())
    }

    fn no_prefix_parse_error(&mut self, token: Token) {
        self.errors
            .push(format!("no prefix parse fn for {} found", token))
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

    fn expect_peek_(&mut self, token: Token) -> Result<(), Token> {
        if self.peek_token == token {
            self.next_token();
            Ok(())
        } else {
            self.peek_error(token);
            Err(self.peek_token.clone())
        }
    }

    fn expect_curr_(&mut self, token: Token) -> Result<(), Token> {
        if self.curr_token == token {
            self.next_token();
            Ok(())
        } else {
            Err(self.peek_token.clone())
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
            "expected next token to be {} got {} instead",
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

    fn print_tokens(&self) {
        println!(
            "Current token: {} Peek token: {}",
            self.curr_token, self.peek_token
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn format_errors(errors: Vec<String>) -> String {
        errors
            .iter()
            .enumerate()
            .fold(format!(""), |acc, (i, err)| {
                format!("{}\n{}: {}", acc, i + 1, err)
            })
    }

    #[test]
    fn test_let() {
        let input = "
        let x = 5;
        let y = 10 + 5;
        let z = \"foo\";
        let foobar = 838383;
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(
            program.statements[0],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("x"))),
                value: Expression::IntegerLiteral(Token::Int(5))
            })
        );
        assert_eq!(
            program.statements[1],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("y"))),
                value: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Plus,
                    left: Expression::IntegerLiteral(Token::Int(10)),
                    right: Expression::IntegerLiteral(Token::Int(5))
                }))
            })
        );
        assert_eq!(
            program.statements[2],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("z"))),
                value: Expression::StringLiteral(Token::String(String::from("foo")))
            })
        );
        assert_eq!(
            program.statements[3],
            Statement::Let(LetStatement {
                name: Identifier(Token::Ident(String::from("foobar"))),
                value: Expression::IntegerLiteral(Token::Int(838383))
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
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert!(program.statements.len() == 3);
        assert_eq!(
            program.statements[0],
            Statement::Return(ReturnStatement {
                return_value: Expression::IntegerLiteral(Token::Int(5))
            })
        );
        assert_eq!(
            program.statements[1],
            Statement::Return(ReturnStatement {
                return_value: Expression::IntegerLiteral(Token::Int(10))
            })
        );
        assert_eq!(
            program.statements[2],
            Statement::Return(ReturnStatement {
                return_value: Expression::IntegerLiteral(Token::Int(993322))
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
            panic!("Parsing errors: {}", format_errors(p.errors));
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
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 1);
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::IntegerLiteral(Token::Int(5)))
        );
    }

    #[test]
    fn test_expr_string() {
        let input = "
        \"fooo\";
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::StringLiteral(Token::String(String::from(
                "fooo"
            ))))
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
            panic!("Parsing errors: {}", format_errors(p.errors));
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
            panic!("Parsing errors: {}", format_errors(p.errors));
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
            panic!("Parsing errors: {}", format_errors(p.errors));
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
                Statement::Expression(Expression::Operation(Box::new(InfixExpression {
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
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 2);

        // (1 < (2 - (3 * 4))) > 5;
        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Operation(Box::new(InfixExpression {
                operator: Token::Gt,
                left: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Lt,
                    left: Expression::Prefix(Box::new(PrefixExpression {
                        operator: Token::Minus,
                        right: Expression::IntegerLiteral(Token::Int(1)),
                    })),
                    right: Expression::Operation(Box::new(InfixExpression {
                        operator: Token::Minus,
                        left: Expression::IntegerLiteral(Token::Int(2)),
                        right: Expression::Operation(Box::new(InfixExpression {
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
            Statement::Expression(Expression::Operation(Box::new(InfixExpression {
                operator: Token::Equals,
                left: Expression::Operation(Box::new(InfixExpression {
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
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Operation(Box::new(InfixExpression {
                operator: Token::Plus,
                left: Expression::IntegerLiteral(Token::Int(1)),
                right: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Minus,
                    left: Expression::IntegerLiteral(Token::Int(2)),
                    right: Expression::IntegerLiteral(Token::Int(3))
                })),
            })))
        );
    }

    #[test]
    fn test_if_expr() {
        let input = "
        if (x < y) { x }
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::If(Box::new(IfExpression {
                condition: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Lt,
                    left: Expression::Ident(Token::Ident(String::from("x"))),
                    right: Expression::Ident(Token::Ident(String::from("y"))),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident(Token::Ident(
                        String::from("x")
                    )))]
                },
                alternative: BlockStatement { statements: vec![] }
            })))
        );
    }

    #[test]
    fn test_if_else_expr() {
        let input = "
        if (x < y) { x } else { y }
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 1);

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::If(Box::new(IfExpression {
                condition: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Lt,
                    left: Expression::Ident(Token::Ident(String::from("x"))),
                    right: Expression::Ident(Token::Ident(String::from("y"))),
                })),
                consequence: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident(Token::Ident(
                        String::from("x")
                    )))]
                },
                alternative: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident(Token::Ident(
                        String::from("y")
                    )))]
                },
            })))
        );
    }

    #[test]
    fn test_fn() {
        let input = "
        fn() { 1; };
        fn(x) { x; };
        fn(x, y, z) { x + y; }
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(program.statements.len(), 3);

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Fn(FunctionLiteral {
                parameters: vec![],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::IntegerLiteral(
                        Token::Int(1)
                    ))]
                }
            }))
        );
        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::Fn(FunctionLiteral {
                parameters: vec![Identifier(Token::Ident(String::from("x")))],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Ident(Token::Ident(
                        String::from("x")
                    )))]
                }
            }))
        );
        assert_eq!(
            program.statements[2],
            Statement::Expression(Expression::Fn(FunctionLiteral {
                parameters: vec![
                    Identifier(Token::Ident(String::from("x"))),
                    Identifier(Token::Ident(String::from("y"))),
                    Identifier(Token::Ident(String::from("z"))),
                ],
                body: BlockStatement {
                    statements: vec![Statement::Expression(Expression::Operation(Box::new(
                        InfixExpression {
                            operator: Token::Plus,
                            left: Expression::Ident(Token::Ident(String::from("x"))),
                            right: Expression::Ident(Token::Ident(String::from("y"))),
                        }
                    )))]
                }
            }))
        );
    }

    #[test]
    fn test_call_expr() {
        let input = "
        a();
        a(1, x);
        a(x + y, 2 - b());
        fn() { 1; }();
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Call(Box::new(CallExpression {
                arguments: vec![],
                function: Expression::Ident(Token::Ident(String::from("a")))
            })))
        );
        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::Call(Box::new(CallExpression {
                arguments: vec![
                    Expression::IntegerLiteral(Token::Int(1)),
                    Expression::Ident(Token::Ident(String::from("x")))
                ],
                function: Expression::Ident(Token::Ident(String::from("a")))
            })))
        );
        assert_eq!(
            program.statements[2],
            Statement::Expression(Expression::Call(Box::new(CallExpression {
                arguments: vec![
                    Expression::Operation(Box::new(InfixExpression {
                        operator: Token::Plus,
                        left: Expression::Ident(Token::Ident(String::from("x"))),
                        right: Expression::Ident(Token::Ident(String::from("y"))),
                    })),
                    Expression::Operation(Box::new(InfixExpression {
                        operator: Token::Minus,
                        left: Expression::IntegerLiteral(Token::Int(2)),
                        right: Expression::Call(Box::new(CallExpression {
                            function: Expression::Ident(Token::Ident(String::from("b"))),
                            arguments: vec![]
                        })),
                    })),
                ],
                function: Expression::Ident(Token::Ident(String::from("a")))
            })))
        );
        assert_eq!(
            program.statements[3],
            Statement::Expression(Expression::Call(Box::new(CallExpression {
                arguments: vec![],
                function: Expression::Fn(FunctionLiteral {
                    parameters: vec![],
                    body: BlockStatement {
                        statements: vec![Statement::Expression(Expression::IntegerLiteral(
                            Token::Int(1)
                        ))]
                    }
                })
            })))
        );
    }

    #[test]
    fn test_array() {
        let input = "
        [1, \"2\", 3 + 3, fn(x) { x }, add(2, 2)]
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::Array(Box::new(Array {
                elements: vec![
                    Expression::IntegerLiteral(Token::Int(1)),
                    Expression::StringLiteral(Token::String("2".to_string())),
                    Expression::Operation(Box::new(InfixExpression {
                        operator: Token::Plus,
                        left: Expression::IntegerLiteral(Token::Int(3)),
                        right: Expression::IntegerLiteral(Token::Int(3))
                    })),
                    Expression::Fn(FunctionLiteral {
                        parameters: vec![Identifier(Token::Ident(String::from("x")))],
                        body: BlockStatement {
                            statements: vec![Statement::Expression(Expression::Ident(
                                Token::Ident(String::from("x"))
                            ))]
                        }
                    }),
                    Expression::Call(Box::new(CallExpression {
                        arguments: vec![
                            Expression::IntegerLiteral(Token::Int(2)),
                            Expression::IntegerLiteral(Token::Int(2)),
                        ],
                        function: Expression::Ident(Token::Ident(String::from("add")))
                    }))
                ]
            })))
        );
    }

    #[test]
    fn test_array_access() {
        let input = "
        arr[0];
        arr[1 + a];
        ";
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        if !p.errors.is_empty() {
            panic!("Parsing errors: {}", format_errors(p.errors));
        }

        assert_eq!(
            program.statements[0],
            Statement::Expression(Expression::PropertyAccess(Box::new(PropertyAccess {
                property: Expression::IntegerLiteral(Token::Int(0)),
                object: Expression::Ident(Token::Ident("arr".to_string()))
            })))
        );

        assert_eq!(
            program.statements[1],
            Statement::Expression(Expression::PropertyAccess(Box::new(PropertyAccess {
                property: Expression::Operation(Box::new(InfixExpression {
                    operator: Token::Plus,
                    left: Expression::IntegerLiteral(Token::Int(1)),
                    right: Expression::Ident(Token::Ident("a".to_string()))
                })),
                object: Expression::Ident(Token::Ident("arr".to_string()))
            })))
        );
    }
}
