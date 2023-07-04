use std::fmt::{self, Display};

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
    BlockStatement(BlockStatement),
}

impl From<Statement> for Node {
    fn from(value: Statement) -> Self {
        Node::Statement(value)
    }
}

impl From<Expression> for Node {
    fn from(value: Expression) -> Self {
        Node::Expression(value)
    }
}

impl From<Program> for Node {
    fn from(value: Program) -> Self {
        Node::Program(value)
    }
}

impl From<BlockStatement> for Node {
    fn from(value: BlockStatement) -> Self {
        Node::BlockStatement(value)
    }
}

// pub trait Node {
//     fn get_node(&self) -> Self;
// }

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Token),
    IntegerLiteral(Token),
    BooleanLiteral(Token),
    Fn(FunctionLiteral),
    Prefix(Box<PrefixExpression>),
    Operation(Box<InfixExpression>),
    If(Box<IfExpression>),
    Call(Box<CallExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub operator: Token,
    pub right: Expression,
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

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub operator: Token,
    pub left: Expression,
    pub right: Expression,
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: BlockStatement,
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if ({}) {{{}}} else {{{}}})",
            self.condition, self.consequence, self.alternative
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.function.clone() {
            Expression::Ident(t) => write!(f, "{}(", t)?,
            Expression::Fn(fun) => write!(f, "{}(", fun)?,
            _ => panic!("function name should be identifier or function"),
        }

        for (i, arg) in self.arguments.iter().enumerate() {
            if i == self.arguments.len() - 1 {
                write!(f, "{})", arg)?
            } else {
                write!(f, "{},", arg)?
            }
        }

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{:?}", stmt)?;
        }
        Ok(())
    }
}

impl Default for BlockStatement {
    fn default() -> Self {
        BlockStatement { statements: vec![] }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        for param in self.parameters.iter() {
            write!(f, "{:?}", param)?;
        }
        write!(f, ") {}", self.body)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub Token);

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral(pub Token);

#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral(pub Token);

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}
