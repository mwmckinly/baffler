#![allow(unconditional_recursion)]

use std::fs::read_to_string;

use analyzer::Analyzer;
use lexer::Lexer;
use logger::Logger;
use parser::Parser;

mod token;
mod lexer;
mod logger;
mod utils;
mod syntax;
mod parser;
mod analyzer;

fn main() {
	let filename = "app/main.ori".to_string();
	let source = read_to_string(&filename).unwrap();

	let logger = Logger::new(filename, source);
	let lexer = Lexer::new(logger);
	let parser = Parser::init(lexer);
	let analyzer = Analyzer::init(parser);

	analyzer.analyze();
}
