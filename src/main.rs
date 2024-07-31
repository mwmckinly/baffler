#![allow(non_upper_case_globals)]

use std::fs::write;

use lexer::Lexer;
use parser::Parser;
use util::File;

mod token;
mod lexer;
mod util;
mod syntax;
mod parser;
mod visitor;

fn main() {
	let file = File::new("app/main.baf".into());
	
	let mut lexer = Lexer::init(file.clone());
	let mut parser = Parser::init(&mut lexer);

	write(file.name() + ".ast", serde_yaml::to_string(&parser.parse()).unwrap()).unwrap();
}