use std::fs::read_to_string;

use lexer::Lexer;
use logger::Logger;

mod token;
mod lexer;
mod logger;
mod utils;

fn main() {
	let filename = "app/main.ori".to_string();
	let source = read_to_string(&filename).unwrap();

	let logger = Logger::new(filename, source);
	let lexer = Lexer::new(logger);

	for i in lexer.tokenize() {
		println!("{i}")
	}
}
