#![allow(non_snake_case)]

use std::{error::Error, fs::read_to_string, io::{self, Write as _}};

use lexer::Lexer;
use logger::Logger;
use parser::Parser;
use runtime::Runtime;
use utils::Color;
mod token;
mod lexer;
mod logger;
mod utils;
mod syntax;
mod parser;
mod runtime;
mod envirnment;

fn input(prompt: &str) -> String {
	print!("{}", prompt); 
	io::stdout().flush().unwrap();
	let mut buffer = String::new();
	
	if let Err(err) = io::stdin().read_line(&mut buffer) {
		println!("{err}")
	};

	return buffer.trim().to_string();
}

fn get_content() -> [String; 2] {
	let filename = if let Some(arg) = std::env::args().nth(1) { arg } else {
		input("filename: ")
	};

	let source = match std::fs::read_to_string(&filename) {
		Ok(src) => src,
		Err(err) => {
			println!("{}: {err}", "error".color(31));
			std::process::exit(1);
		},
	};

	[filename, source]
}

fn main() {
	let [filename, source] = get_content();
	let logger = Logger::new(filename, source);

	let lexer = Lexer::new(logger);
	let parser = Parser::init(lexer);
	let runtime = Runtime::init(parser);

	runtime.interperate();
}
