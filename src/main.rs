use std::io;
use std::io::Write;

use crate::lexer::Lexer;
use crate::parser::Parser;

mod lexer;
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

        println!("{:?}", program);
    }
}
