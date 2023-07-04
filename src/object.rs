#![allow(dead_code)]

pub trait Inspectable {
    fn inspect(&self) -> String;
}

pub enum ObjectEnum {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
}

impl Inspectable for ObjectEnum {
    fn inspect(&self) -> String {
        match self {
            ObjectEnum::Integer(obj) => Integer::inspect(obj),
            ObjectEnum::Boolean(obj) => Boolean::inspect(obj),
            ObjectEnum::Null(obj) => Null::inspect(obj),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Integer {
    pub value: isize,
}

impl Integer {
    pub fn new(value: isize) -> Self {
        Integer { value }
    }
}

impl Inspectable for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

impl Boolean {
    pub fn new(value: bool) -> Self {
        Boolean { value }
    }
}

impl Inspectable for Boolean {
    fn inspect(&self) -> String {
        format!("{}", self.value)
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
