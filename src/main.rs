#![allow(non_upper_case_globals)]

use std::fs::write;

use checker::TypeChecker;
use lexer::Lexer;
use parser::Parser;
use syntax::Node;
use util::{Color, File};

mod token;
mod lexer;
mod util;
mod syntax;
mod parser;
mod runtime;
mod checker;

fn main() {
	let file = File::new("app/main.baf".into());

	analyze(file);
}

#[allow(unused)]
fn parser(file: File) {
	let mut lexer = Lexer::init(file.clone());
	let mut parser = Parser::init(&mut lexer);

	write(file.name() + ".ast", serde_yaml::to_string(&parser.parse()).unwrap()).unwrap();
}
#[allow(unused)]
fn analyze(file: File) {
	let mut lexer = Lexer::init(file);
	let mut parser = Parser::init(&mut lexer);

	let nodes = if let Node::Compound { body } = parser.parse() {
		body
	} else { panic!(); };

	let mut analyzer = TypeChecker::init(&mut parser);

	analyzer.message(&nodes[0], "info".color(36), "", "message");
}