#![allow(dead_code)]

use std::{
    collections::HashMap,
    fmt::{self, Display},
};

use crate::ast::{BlockStatement, Identifier};

pub trait Inspectable {
    fn inspect(&self) -> String;
    fn inspect_type(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum ObjectEnum {
    Integer(Integer),
    String(StringObj),
    Boolean(Boolean),
    Null(Null),
    Return(Box<ReturnValue>),
    Error(Error),
    Function(Function),
}

impl Inspectable for ObjectEnum {
    fn inspect(&self) -> String {
        match self {
            ObjectEnum::Integer(obj) => Integer::inspect(obj),
            ObjectEnum::String(obj) => StringObj::inspect(obj),
            ObjectEnum::Boolean(obj) => Boolean::inspect(obj),
            ObjectEnum::Null(obj) => Null::inspect(obj),
            ObjectEnum::Return(obj) => ReturnValue::inspect(obj),
            ObjectEnum::Error(obj) => Error::inspect(obj),
            ObjectEnum::Function(obj) => Function::inspect(obj),
        }
    }

    fn inspect_type(&self) -> String {
        match self {
            ObjectEnum::Integer(obj) => Integer::inspect_type(obj),
            ObjectEnum::String(obj) => StringObj::inspect_type(obj),
            ObjectEnum::Boolean(obj) => Boolean::inspect_type(obj),
            ObjectEnum::Null(obj) => Null::inspect_type(obj),
            ObjectEnum::Return(obj) => ReturnValue::inspect_type(obj),
            ObjectEnum::Error(obj) => Error::inspect_type(obj),
            ObjectEnum::Function(obj) => Function::inspect_type(obj),
        }
    }
}

impl Display for ObjectEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect_type())
    }
}

impl PartialEq for ObjectEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ObjectEnum::Integer(x), ObjectEnum::Integer(y)) => x.0 == y.0,
            (ObjectEnum::Boolean(x), ObjectEnum::Boolean(y)) => x.0 == y.0,
            _ => false,
        }
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl From<Integer> for ObjectEnum {
    fn from(value: Integer) -> Self {
        ObjectEnum::Integer(value)
    }
}

impl From<Boolean> for ObjectEnum {
    fn from(value: Boolean) -> Self {
        ObjectEnum::Boolean(value)
    }
}

impl From<Null> for ObjectEnum {
    fn from(value: Null) -> Self {
        ObjectEnum::Null(value)
    }
}

impl From<ReturnValue> for ObjectEnum {
    fn from(value: ReturnValue) -> Self {
        ObjectEnum::Return(Box::new(value))
    }
}

impl From<Error> for ObjectEnum {
    fn from(value: Error) -> Self {
        ObjectEnum::Error(value)
    }
}

impl From<Function> for ObjectEnum {
    fn from(value: Function) -> Self {
        ObjectEnum::Function(value)
    }
}

impl From<StringObj> for ObjectEnum {
    fn from(value: StringObj) -> Self {
        ObjectEnum::String(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer(pub isize);

impl Inspectable for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.0)
    }

    fn inspect_type(&self) -> String {
        format!("INTEGER")
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect_type())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringObj(pub String);

impl Inspectable for StringObj {
    fn inspect(&self) -> String {
        format!("{}", self.0)
    }

    fn inspect_type(&self) -> String {
        format!("STRING")
    }
}

impl Display for StringObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect_type())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Boolean(pub bool);

impl Inspectable for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.0)
    }

    fn inspect_type(&self) -> String {
        format!("BOOLEAN")
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect_type())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnValue(pub ObjectEnum);

impl Inspectable for ReturnValue {
    fn inspect(&self) -> String {
        format!("{}", self.0.inspect())
    }

    fn inspect_type(&self) -> String {
        self.0.inspect_type()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Null {}

impl Inspectable for Null {
    fn inspect(&self) -> String {
        format!("null")
    }

    fn inspect_type(&self) -> String {
        format!("NULL")
    }
}

impl Default for Null {
    fn default() -> Self {
        Null {}
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    message: String,
}

impl Error {
    pub fn new(message: String) -> Self {
        Error { message }
    }
}

impl Inspectable for Error {
    fn inspect(&self) -> String {
        self.message.clone()
    }

    fn inspect_type(&self) -> String {
        self.inspect()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Environment,
}

impl Function {
    pub fn new(parameters: Vec<Identifier>, body: BlockStatement, env: Environment) -> Self {
        Function {
            parameters,
            body,
            env,
        }
    }
}

impl Inspectable for Function {
    fn inspect(&self) -> String {
        let mut buf = String::from("fn(");

        let mut parameters_iter = self.parameters.clone().into_iter();
        let first = parameters_iter.nth(0);

        if let Some(param) = first {
            buf.push_str(&format!("{}", param));
        } else {
            return buf;
        }

        for param in parameters_iter {
            buf.push_str(&format!(",{}", param));
        }

        buf.push_str(") { ");

        buf.push_str(&self.body.to_string());

        buf.push_str(" }");

        buf
    }

    fn inspect_type(&self) -> String {
        self.inspect()
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, ObjectEnum>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_with_env(outer: Environment) -> Self {
        Environment {
            store: HashMap::new(),
            outer: Some(Box::new(outer)),
        }
    }

    pub fn get(&self, key: String) -> Option<&ObjectEnum> {
        let val = self.store.get(&key);

        if let Some(v) = val {
            return Some(v);
        }

        if let Some(outer) = &self.outer {
            return outer.get(key);
        }

        return None;
    }

    pub fn set(&mut self, key: String, val: ObjectEnum) -> Option<ObjectEnum> {
        self.store.insert(key, val)
    }
}
