#![allow(dead_code)]

pub trait Inspectable {
    fn inspect(&self) -> String;
}

#[derive(Debug)]
pub enum ObjectEnum {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    Return(Box<ReturnValue>),
}

impl Inspectable for ObjectEnum {
    fn inspect(&self) -> String {
        match self {
            ObjectEnum::Integer(obj) => Integer::inspect(obj),
            ObjectEnum::Boolean(obj) => Boolean::inspect(obj),
            ObjectEnum::Null(obj) => Null::inspect(obj),
            ObjectEnum::Return(obj) => ReturnValue::inspect(obj),
        }
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

#[derive(Debug, PartialEq)]
pub struct Integer(pub isize);

impl Inspectable for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct Boolean(pub bool);

impl Inspectable for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.0)
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnValue(ObjectEnum);

impl Inspectable for ReturnValue {
    fn inspect(&self) -> String {
        format!("{}", self.0.inspect())
    }
}

#[derive(Debug, PartialEq)]
pub struct Null {}

impl Inspectable for Null {
    fn inspect(&self) -> String {
        format!("null")
    }
}

impl Default for Null {
    fn default() -> Self {
        Null {}
    }
}
