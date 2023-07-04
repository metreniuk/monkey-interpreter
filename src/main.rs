use std::io;
use std::io::Write;

use crate::evaluator::eval;
use crate::lexer::Lexer;
use crate::object::Inspectable;
use crate::parser::Parser;

mod ast;
mod evaluator;
mod lexer;
mod object;
mod parser;

pub fn do_work() -> bool {
    todo!("implement later");
}

fn main() -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!\n");

    loop {
        print!(">>> ");
        io::stdout().flush()?;
        let mut buf = String::new();
        io::stdin().read_line(&mut buf)?;

        let l = Lexer::new(buf);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        let output = eval(ast::Node::Program(program));

        let str = output.inspect();

        println!("{}", str);
    }
}
