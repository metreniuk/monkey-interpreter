#![allow(dead_code)]

use crate::{
    ast::{Expression, IfExpression, Node, Statement},
    lexer::Token,
    object::{Boolean, Integer, Null, ObjectEnum},
};

const TRUE: Boolean = Boolean(true);
const FALSE: Boolean = Boolean(false);
const NULL: Null = Null {};

pub fn eval(node: Node) -> ObjectEnum {
    match node {
        Node::Expression(exp) => match exp {
            Expression::IntegerLiteral(Token::Int(value)) => Integer(value).into(),
            // TODO: make boolean literal contain only true and false
            Expression::BooleanLiteral(Token::True) => TRUE.into(),
            Expression::BooleanLiteral(Token::False) => FALSE.into(),
            Expression::Prefix(expr) => {
                let right = eval(expr.right.into());
                eval_prefix_expression(expr.operator, right)
            }
            Expression::Operation(expr) => {
                let left = eval(expr.left.into());
                let right = eval(expr.right.into());
                eval_infix_expression(expr.operator, left, right)
            }
            Expression::If(expr) => eval_if_expression(*expr),
            _ => NULL.into(),
        },

        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => eval(expr.into()),
            _ => NULL.into(),
        },
        Node::BlockStatement(bl) => eval_statements(bl.statements),
        Node::Program(pr) => eval_statements(pr.statements),
    }
}

fn eval_statements(stmts: Vec<Statement>) -> ObjectEnum {
    let mut obj: ObjectEnum = NULL.into();

    for stmt in stmts {
        obj = eval(stmt.into());
    }

    obj
}

fn eval_if_expression(expr: IfExpression) -> ObjectEnum {
    let cond = eval(expr.condition.into());
    if is_truthy(&cond) {
        eval(expr.consequence.into())
    } else if !expr.alternative.statements.is_empty() {
        eval(expr.alternative.into())
    } else {
        NULL.into()
    }
}

fn is_truthy(obj: &ObjectEnum) -> bool {
    match obj {
        ObjectEnum::Boolean(Boolean(false)) | ObjectEnum::Null(_) => false,
        _ => true,
    }
}

fn eval_prefix_expression(operator: Token, right: ObjectEnum) -> ObjectEnum {
    match operator {
        Token::Bang => eval_bang_operator(right).into(),
        Token::Minus => eval_minus_operator(right),
        _ => NULL.into(),
    }
}

fn eval_infix_expression(operator: Token, left: ObjectEnum, right: ObjectEnum) -> ObjectEnum {
    match (left, right) {
        (ObjectEnum::Integer(x), ObjectEnum::Integer(y)) => {
            eval_infix_integer_expression(operator, x, y)
        }
        (ObjectEnum::Boolean(x), ObjectEnum::Boolean(y)) => Boolean(x == y).into(),
        _ => NULL.into(),
    }
}

fn eval_infix_integer_expression(operator: Token, left: Integer, right: Integer) -> ObjectEnum {
    match operator {
        Token::Plus => Integer(left.0 + right.0).into(),
        Token::Asterisk => Integer(left.0 * right.0).into(),
        Token::Minus => Integer(left.0 - right.0).into(),
        Token::Slash => Integer(left.0 / right.0).into(),
        Token::Equals => Boolean(left == right).into(),
        Token::NotEquals => Boolean(left != right).into(),
        Token::Lt => Boolean(left.0 < right.0).into(),
        Token::Gt => Boolean(left.0 > right.0).into(),
        _ => NULL.into(),
    }
}

fn eval_bang_operator(right: ObjectEnum) -> Boolean {
    match right {
        ObjectEnum::Boolean(x) => match x {
            Boolean(true) => FALSE,
            Boolean(false) => TRUE,
        },
        ObjectEnum::Integer(x) => match x {
            Integer(0) => TRUE,
            _ => FALSE,
        },
        _ => TRUE,
    }
}

fn eval_minus_operator(right: ObjectEnum) -> ObjectEnum {
    match right {
        ObjectEnum::Integer(x) => match x {
            Integer(0) => Integer(0).into(),
            Integer(value) => Integer(-value).into(),
        },
        _ => NULL.into(),
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
    fn test_infix_integer_expression() {
        test_program("5;", "5");
        test_program("10;", "10");
        test_program("-5;", "-5");
        test_program("-10;", "-10");
        test_program("5 + 5 + 5 + 5 - 10;", "10");
        test_program("2 * 2 * 2 * 2 * 2;", "32");
        test_program("-50 + 100 + -50;", "0");
        test_program("5 * 2 + 10;", "20");
        test_program("5 + 2 * 10;", "25");
        test_program("20 + 2 * -10;", "0");
        test_program("50 / 2 * 2 + 10;", "60");
        test_program("2 * (5 + 10);", "30");
        test_program("3 * 3 * 3 + 10;", "37");
        test_program("3 * (3 * 3) + 10;", "37");
        test_program("(5 + 10 * 2 + 15 / 3) * 2 + -10;", "50");
    }

    #[test]
    fn test_infix_boolean_expression() {
        test_program("true;", "true");
        test_program("false;", "false");
        test_program("1 < 2;", "true");
        test_program("1 > 2;", "false");
        test_program("1 < 1;", "false");
        test_program("1 > 1;", "false");
        test_program("1 == 1;", "true");
        test_program("1 != 1;", "false");
        test_program("1 == 2;", "false");
        test_program("1 != 2;", "true");
        test_program("(1 < 2) == true;", "true");
        test_program("(1 < 2) == false;", "false");
        test_program("(1 > 2) == true;", "false");
        test_program("(1 > 2) == false;", "true");
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

    #[test]
    fn test_if_else_expression() {
        test_program("if (true) { 10 };", "10");
        test_program("if (false) { 10 };", "null");
        test_program("if (1) { 10 };", "10");
        test_program("if (1 < 2) { 10 };", "10");
        test_program("if (1 > 2) { 10 };", "null");
        test_program("if (1 > 2) { 10 } else { 20 };", "20");
        test_program("if (1 < 2) { 10 } else { 20 };", "10");
    }
}
