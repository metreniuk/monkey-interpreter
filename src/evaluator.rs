#![allow(dead_code)]

use crate::{
    ast::{Expression, IfExpression, Node, Statement},
    lexer::Token,
    object::{
        Boolean, BuiltIn, Environment, Error, Function, Inspectable, Integer, Null, ObjectEnum,
        ReturnValue, StringObj,
    },
};

const TRUE: Boolean = Boolean(true);
const FALSE: Boolean = Boolean(false);
const NULL: Null = Null {};

fn builtin_len(args: Vec<ObjectEnum>) -> ObjectEnum {
    if args.len() != 1 {
        return Error::new(format!(
            "wrong number of arguments. got={}, want=1",
            args.len()
        ))
        .into();
    }

    match args.first().unwrap().clone() {
        ObjectEnum::String(x) => Integer(x.0.len().try_into().unwrap()).into(),
        x => Error::new(format!(
            "argument to \"len\" not supported, got {}",
            x.inspect_type()
        ))
        .into(),
    }
}

fn get_builtin(name: String) -> Option<BuiltIn> {
    match name.as_str() {
        "len" => Some(BuiltIn::new("len".into(), builtin_len)),
        _ => None,
    }
}

pub fn eval(node: Node, env: &mut Environment) -> ObjectEnum {
    match node {
        Node::Expression(exp) => match exp {
            Expression::IntegerLiteral(Token::Int(value)) => Integer(value).into(),
            Expression::StringLiteral(Token::String(value)) => StringObj(value).into(),
            // TODO: make boolean literal contain only true and false
            Expression::BooleanLiteral(Token::True) => TRUE.into(),
            Expression::BooleanLiteral(Token::False) => FALSE.into(),
            // TODO: make ident contain only token for ident
            Expression::Ident(token) => eval_identifier(token, env),
            Expression::Prefix(expr) => {
                let right = eval(expr.right.into(), env);
                if let ObjectEnum::Return(_) = right.clone() {
                    return right;
                }
                eval_prefix_expression(expr.operator, right)
            }
            Expression::Operation(expr) => {
                let left = eval(expr.left.into(), env);
                let right = eval(expr.right.into(), env);
                if let ObjectEnum::Return(_) = left.clone() {
                    return left;
                } else if let ObjectEnum::Return(_) = right.clone() {
                    return right;
                }
                eval_infix_expression(expr.operator, left, right)
            }
            Expression::If(expr) => eval_if_expression(*expr, env),
            Expression::Fn(function) => {
                Function::new(function.parameters, function.body, env.clone()).into()
            }
            Expression::Call(node) => {
                let fun = eval(node.function.into(), env);
                if let ObjectEnum::Error(_) = fun {
                    return fun;
                }
                let args = eval_expresssions(node.arguments, env);

                match args {
                    Result::Err(err) => err,
                    Result::Ok(args) => apply_function(fun, args),
                }
            }
            _ => NULL.into(),
        },
        Node::Statement(stmt) => match stmt {
            Statement::Let(expr) => {
                let val = eval(expr.value.into(), env);
                if let ObjectEnum::Error(_) = val.clone() {
                    return val;
                }
                env.set(expr.name.0.to_string(), val.clone());
                val
            }
            Statement::Expression(expr) => {
                let res = eval(expr.into(), env);
                if let ObjectEnum::Return(_) = res.clone() {
                    return res;
                }
                res
            }
            Statement::Return(expr) => {
                let returned = eval(expr.return_value.into(), env);
                if let ObjectEnum::Return(_) = returned.clone() {
                    return returned;
                }
                ReturnValue(returned).into()
            }
        },
        Node::BlockStatement(bl) => eval_statements(bl.statements, env),
        Node::Program(pr) => eval_statements(pr.statements, env),
    }
}

fn eval_statements(stmts: Vec<Statement>, env: &mut Environment) -> ObjectEnum {
    let mut obj: ObjectEnum = NULL.into();

    for stmt in stmts {
        obj = eval(stmt.into(), env);

        match obj {
            ObjectEnum::Return(returned) => {
                return (*returned.clone()).into();
            }
            ObjectEnum::Error(error) => return error.into(),
            _ => {}
        }
    }

    obj
}

fn eval_expresssions(
    expressions: Vec<Expression>,
    env: &mut Environment,
) -> Result<Vec<ObjectEnum>, ObjectEnum> {
    let mut result = vec![];

    for exp in expressions {
        let evaluated = eval(exp.into(), env);
        if let ObjectEnum::Return(_) = evaluated {
            return Result::Err(evaluated);
        }
        result.push(evaluated);
    }

    Ok(result)
}

fn apply_function(fun: ObjectEnum, args: Vec<ObjectEnum>) -> ObjectEnum {
    match fun {
        ObjectEnum::Function(fun) => {
            let mut env = extend_function_env(&fun, args);
            let evaluated = eval(fun.body.into(), &mut env);
            if let ObjectEnum::Return(val) = evaluated {
                val.0
            } else {
                evaluated
            }
        }
        ObjectEnum::BuiltIn(x) => {
            let fun_to_call = x.fun;
            fun_to_call(args)
        }
        x => return Error::new(format!("Not a function: {}", x)).into(),
    }
}

fn extend_function_env(fun: &Function, args: Vec<ObjectEnum>) -> Environment {
    let mut extended_env = Environment::new_with_env(fun.env.clone());

    for (i, param) in fun.parameters.iter().enumerate() {
        extended_env.set(param.to_string(), args[i].clone());
    }

    extended_env
}

fn eval_identifier(token: Token, env: &mut Environment) -> ObjectEnum {
    if let Token::Ident(name) = token {
        let val = env.get(name.clone());
        if let Some(v) = val {
            v.clone()
        } else if let Some(builtin) = get_builtin(name.clone()) {
            builtin.into()
        } else {
            Error::new(format!("Identifier not found: {}", name)).into()
        }
    } else {
        Error::new(format!("Wrong identifier token: {}", token)).into()
    }
}

fn eval_if_expression(expr: IfExpression, env: &mut Environment) -> ObjectEnum {
    let cond = eval(expr.condition.into(), env);

    if let ObjectEnum::Return(_) = cond.clone() {
        return cond;
    }

    if is_truthy(&cond) {
        eval(expr.consequence.into(), env)
    } else if !expr.alternative.statements.is_empty() {
        eval(expr.alternative.into(), env)
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
        _ => Error::new(format!("Unkown operator: {} {}", operator, right)).into(),
    }
}

fn eval_infix_expression(operator: Token, left: ObjectEnum, right: ObjectEnum) -> ObjectEnum {
    match (left, right) {
        (ObjectEnum::Integer(x), ObjectEnum::Integer(y)) => {
            eval_infix_integer_expression(operator, x, y)
        }
        (ObjectEnum::String(x), ObjectEnum::String(y)) => {
            eval_infix_string_expression(operator, x, y)
        }
        (ObjectEnum::Boolean(x), ObjectEnum::Boolean(y)) => match operator {
            Token::Equals | Token::NotEquals => Boolean(x == y).into(),
            op => Error::new(format!("Unknown operator: {} {} {}", x, op, y)).into(),
        },
        (x, y) => Error::new(format!("Type mismatch: {} {} {}", x, operator, y)).into(),
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
        x => Error::new(format!("Unknown operator: {} {} {}", left, x, right)).into(),
    }
}

fn eval_infix_string_expression(operator: Token, left: StringObj, right: StringObj) -> ObjectEnum {
    match operator {
        Token::Plus => StringObj(left.0 + &right.0).into(),
        x => Error::new(format!("Unknown operator: {} {} {}", left, x, right)).into(),
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
        x => Error::new(format!("Unknown operator: {}{}", Token::Minus, x)).into(),
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
        let mut env = Environment::new();

        let obj = eval(Node::Program(program), &mut env);
        assert_eq!(obj.inspect(), target);
    }

    #[test]
    fn test_int() {
        test_program("5;", "5");
        test_program("-5;", "-5");
    }

    #[test]
    fn test_string() {
        test_program("\"foo\";", "foo");
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
    fn test_infix_string_expression() {
        test_program("\"foo\" + \" \" + \"bar\"", "foo bar");
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

    #[test]
    fn test_return_statements() {
        test_program("return 10;", "10");
        test_program("return 10; 9;", "10");
        test_program("return 2 * 5; 9;", "10");
        test_program("9; return 2 * 5; 9;", "10");
        test_program(
            "
            if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1; 
            }
        ",
            "10",
        )
    }

    #[test]
    fn test_error() {
        test_program("5 + true;", "Type mismatch: INTEGER + BOOLEAN");
        test_program("5 + true; 5;", "Type mismatch: INTEGER + BOOLEAN");
        test_program("5 + \"foo\";", "Type mismatch: INTEGER + STRING");
        test_program("\"foo\" + true;", "Type mismatch: STRING + BOOLEAN");
        test_program("\"foo\" - \"bar\";", "Unknown operator: STRING - STRING");
        test_program("-true;", "Unknown operator: -BOOLEAN");
        test_program("true + false;", "Unknown operator: BOOLEAN + BOOLEAN");
        test_program(
            "if (10 > 1) { true + false; };",
            "Unknown operator: BOOLEAN + BOOLEAN",
        );
        test_program(
            "
        if (10 > 1) {
          if (10 > 1) {
            return true + false;
          }
        return 1; }
        ",
            "Unknown operator: BOOLEAN + BOOLEAN",
        );
        test_program("foobar;", "Identifier not found: foobar");
    }

    #[test]
    fn test_let_statements() {
        test_program("let a = 5; a;", "5");
        test_program("let a = 5 * 5; a;", "25");
        test_program("let a = 5; let b = a; b;", "5");
        test_program("let a = 5; let b = a; let c = a + b + 5; c;", "15");
    }

    #[test]
    fn test_function() {
        test_program("fn(x) { x + 2; };", "fn(x) { (x + 2) }")
    }

    #[test]
    fn test_function_application() {
        test_program("let identity = fn(x) { x; }; identity(5);", "5");
        test_program("let identity = fn(x) { return x; }; identity(5);", "5");
        test_program("let double = fn(x) { x * 2; }; double(5);", "10");
        test_program("let add = fn(x, y) { x + y; }; add(5, 5);", "10");
        test_program(
            "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            "20",
        );
        test_program("fn(x) { x; }(5)", "5");
    }

    #[test]
    fn test_function_clojures() {
        test_program(
            "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);",
            "4",
        );
        test_program(
            "
        let add = fn(a, b) { a + b };
        let applyFunc = fn(a, b, func) { func(a, b) };
        applyFunc(2, 2, add);
        ",
            "4",
        )
    }

    #[test]
    fn test_builtin_len() {
        test_program("len(\"\")", "0");
        test_program("len(\"four\")", "4");
        test_program("len(\"hello world\")", "11");
        test_program("len(1)", "argument to \"len\" not supported, got INTEGER");
        test_program(
            "len(\"one\", \"two\")",
            "wrong number of arguments. got=2, want=1",
        );
    }
}
