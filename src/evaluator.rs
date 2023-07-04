#![allow(dead_code)]

use crate::{
    ast::{Expression, IfExpression, Node, Statement},
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
                let right = eval(expr.right.into());
                eval_prefix_expression(expr.operator, right)
            }
            Expression::Operation(expr) => {
                let left = eval(expr.left.into());
                let right = eval(expr.right.into());
                eval_infix_expression(expr.operator, left, right)
            }
            Expression::If(expr) => eval_if_expression(*expr),
            _ => ObjectEnum::Null(NULL),
        },

        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => eval(expr.into()),
            _ => ObjectEnum::Null(NULL),
        },
        Node::BlockStatement(bl) => eval_statements(bl.statements),
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

fn eval_if_expression(expr: IfExpression) -> ObjectEnum {
    let cond = eval(expr.condition.into());
    if is_truthy(&cond) {
        eval(expr.consequence.into())
    } else if !expr.alternative.statements.is_empty() {
        eval(expr.alternative.into())
    } else {
        ObjectEnum::Null(NULL)
    }
}

fn is_truthy(obj: &ObjectEnum) -> bool {
    match obj {
        ObjectEnum::Boolean(Boolean { value: false }) | ObjectEnum::Null(_) => false,
        _ => true,
    }
}

fn eval_prefix_expression(operator: Token, right: ObjectEnum) -> ObjectEnum {
    match operator {
        Token::Bang => ObjectEnum::Boolean(eval_bang_operator(right)),
        Token::Minus => eval_minus_operator(right),
        _ => ObjectEnum::Null(NULL),
    }
}

fn eval_infix_expression(operator: Token, left: ObjectEnum, right: ObjectEnum) -> ObjectEnum {
    match (left, right) {
        (ObjectEnum::Integer(x), ObjectEnum::Integer(y)) => {
            eval_infix_integer_expression(operator, x, y)
        }
        (ObjectEnum::Boolean(x), ObjectEnum::Boolean(y)) => {
            ObjectEnum::Boolean(Boolean { value: x == y })
        }
        _ => ObjectEnum::Null(NULL),
    }
}

fn eval_infix_integer_expression(operator: Token, left: Integer, right: Integer) -> ObjectEnum {
    match operator {
        Token::Plus => ObjectEnum::Integer(Integer {
            value: left.value + right.value,
        }),
        Token::Asterisk => ObjectEnum::Integer(Integer {
            value: left.value * right.value,
        }),
        Token::Minus => ObjectEnum::Integer(Integer {
            value: left.value - right.value,
        }),
        Token::Slash => ObjectEnum::Integer(Integer {
            value: left.value / right.value,
        }),
        Token::Equals => ObjectEnum::Boolean(Boolean {
            value: left == right,
        }),
        Token::NotEquals => ObjectEnum::Boolean(Boolean {
            value: left != right,
        }),
        Token::Lt => ObjectEnum::Boolean(Boolean {
            value: left.value < right.value,
        }),
        Token::Gt => ObjectEnum::Boolean(Boolean {
            value: left.value > right.value,
        }),
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
