#![allow(unconditional_recursion)]

use std::fs::read_to_string;

use lexer::Lexer;
use logger::Logger;
use parser::Parser;
use runtime::Runtime;
mod token;
mod lexer;
mod logger;
mod utils;
mod syntax;
mod parser;
mod analyzer;
mod runtime;

fn main() {
	let filename = "app/main.ori".to_string();
	let source = read_to_string(&filename).unwrap();

	let logger = Logger::new(filename, source);
	let lexer = Lexer::new(logger);
	let parser = Parser::init(lexer);
	let runtime = Runtime::init(parser);

	runtime.interperate();
}
