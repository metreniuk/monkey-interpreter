#![allow(dead_code)]

use crate::{
    ast::{Expression, Node, Statement},
    lexer::Token,
    object::{Boolean, Integer, Null, ObjectEnum},
};

const TRUE: Boolean = Boolean { value: true };
const FALSE: Boolean = Boolean { value: false };
const NULL: Null = Null {};

pub fn eval(node: Node) -> ObjectEnum {
    match node {
        Node::Expression(exp) => match exp {
            Expression::IntegerLiteral(Token::Int(value)) => {
                ObjectEnum::Integer(Integer::new(value))
            }
            // TODO: make boolean literal contain only true and false
            Expression::BooleanLiteral(Token::True) => ObjectEnum::Boolean(TRUE),
            Expression::BooleanLiteral(Token::False) => ObjectEnum::Boolean(FALSE),
            Expression::Prefix(expr) => {
                let right = eval(Node::Expression(expr.right));
                eval_prefix_expression(expr.operator, right)
            }
            _ => ObjectEnum::Null(NULL),
        },
        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => eval(Node::Expression(expr)),
            _ => ObjectEnum::Null(NULL),
        },
        Node::Program(pr) => eval_statements(pr.statements),
    }
}

fn eval_statements(stmts: Vec<Statement>) -> ObjectEnum {
    let mut obj: ObjectEnum = ObjectEnum::Null(NULL);

    for stmt in stmts {
        obj = eval(Node::Statement(stmt));
    }

    obj
}

fn eval_prefix_expression(operator: Token, right: ObjectEnum) -> ObjectEnum {
    match operator {
        Token::Bang => ObjectEnum::Boolean(eval_bang_operator(right)),
        Token::Minus => eval_minus_operator(right),
        _ => ObjectEnum::Null(NULL),
    }
}

fn eval_bang_operator(right: ObjectEnum) -> Boolean {
    match right {
        ObjectEnum::Boolean(x) => match x {
            Boolean { value: true } => FALSE,
            Boolean { value: false } => TRUE,
        },
        ObjectEnum::Integer(x) => match x {
            Integer { value: 0 } => TRUE,
            _ => FALSE,
        },
        ObjectEnum::Null(_) => TRUE,
    }
}

fn eval_minus_operator(right: ObjectEnum) -> ObjectEnum {
    match right {
        ObjectEnum::Integer(x) => match x {
            Integer { value: 0 } => ObjectEnum::Integer(Integer { value: 0 }),
            Integer { value } => ObjectEnum::Integer(Integer { value: -value }),
        },
        _ => ObjectEnum::Null(NULL),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, object::Inspectable, parser::Parser};

    use super::*;

    fn test_program(input: &str, target: &str) {
        let l = Lexer::new(String::from(input));
        let mut p = Parser::new(l);
        let program = p.parse_program();

        let obj = eval(Node::Program(program));
        assert_eq!(obj.inspect(), target);
    }

    #[test]
    fn test_int() {
        test_program("5;", "5");
        test_program("-5;", "-5");
    }

    #[test]
    fn test_bool() {
        test_program("false;", "false");
        test_program("true;", "true");
        test_program("false; true; false;", "false")
    }

    #[test]
    fn test_bang_operator() {
        test_program("!false;", "true");
        test_program("!true;", "false");
        test_program("!5;", "false");
        test_program("!0;", "true");
        test_program("!!false;", "false");
        test_program("!!true;", "true");
        test_program("!!5;", "true");
        test_program("!!0;", "false");
    }
}
