#![allow(non_upper_case_globals)]

use std::fs::write;

use lexer::Lexer;
use parser::Parser;

mod token;
mod lexer;
mod util;
mod syntax;
mod parser;

fn main() {
	let mut lexer = Lexer::init("app/main.baf".into());
	let mut parser = Parser::init(&mut lexer);

	write(lexer.metadata().0 + ".ast", serde_yaml::to_string(&parser.parse()).unwrap()).unwrap();
}