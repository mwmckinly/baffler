#![allow(non_upper_case_globals)]

use lexer::Lexer;

mod token;
mod lexer;
mod util;
mod syntax;

fn main() {
    let mut lexer = Lexer::init("app/main.baf".into());
    for token in lexer.tokenize() {
        println!("{token}")
    }
}
