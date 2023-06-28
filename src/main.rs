use std::io;
use std::io::Write;

mod lexer;
mod parser;

fn main() -> io::Result<()> {
    println!("Hello! This is the Monkey programming language!\n");

    loop {
        print!(">>> ");
        io::stdout().flush()?;
        let mut buf = String::new();
        io::stdin().read_line(&mut buf)?;

        let mut l = lexer::Lexer::new(buf);
        let mut output = vec![];

        loop {
            let token = l.next_token();
            output.push(token.clone());
            if token == lexer::Token::Eof {
                break;
            }
        }
        println!("{:?}", output);
    }
}
