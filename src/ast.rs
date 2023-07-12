use std::fmt::{self, Display};

use crate::lexer::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
    BlockStatement(BlockStatement),
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::BlockStatement(x) => BlockStatement::fmt(x, f),
            Node::Expression(x) => Expression::fmt(x, f),
            Node::Program(x) => Program::fmt(x, f),
            Node::Statement(x) => Statement::fmt(x, f),
        }
    }
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

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(x) => LetStatement::fmt(x, f),
            Statement::Return(x) => ReturnStatement::fmt(x, f),
            Statement::Expression(x) => Expression::fmt(x, f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(Token),
    IntegerLiteral(Token),
    StringLiteral(Token),
    BooleanLiteral(Token),
    Fn(FunctionLiteral),
    Prefix(Box<PrefixExpression>),
    Operation(Box<InfixExpression>),
    If(Box<IfExpression>),
    Call(Box<CallExpression>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ident(t) => write!(f, "{}", t),
            Expression::IntegerLiteral(t) => write!(f, "{}", t),
            Expression::StringLiteral(t) => write!(f, "{}", t),
            Expression::BooleanLiteral(t) => write!(f, "{}", t),
            Expression::Fn(x) => write!(f, "{}", x),
            Expression::Prefix(x) => write!(f, "{}", x),
            Expression::Operation(x) => write!(f, "{}", x),
            Expression::If(x) => write!(f, "{}", x),
            Expression::Call(x) => write!(f, "{}", x),
        }
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
            "if {} {{ {} }}",
            self.condition,
            self.consequence.to_string(),
        )?;

        if !self.alternative.statements.is_empty() {
            write!(f, " else {{ {} }})", self.alternative.to_string())?
        }

        Ok(())
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
            write!(f, "{}", stmt)?;
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
            write!(f, "{}", param)?;
        }
        write!(f, ") {}", self.body)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for stmt in self.statements.iter() {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "return {};", self.return_value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier(pub Token);

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
