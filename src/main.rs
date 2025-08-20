use crate::{lexer::Lexer, parser::Parser};

mod token;
mod lexer;
mod logger;
mod utilities;
mod blocks;
mod parser;

fn main() {
	let filename: String = "app/main.kr".into();
	let source = std::fs::read_to_string(&filename).unwrap();

	let lexer = Lexer::initialize(filename, source);
	let parser = Parser::init(lexer);

	let (_, blocks) = parser.build_tree();

	for node in blocks {
		println!("{node}")
	}
}
