#![allow(non_upper_case_globals)]

use checker::TypeChecker;
use lexer::Lexer;
use parser::Parser;
use typing::Anaylzer;
use util::File;

mod token;
mod lexer;
mod util;
mod syntax;
mod parser;
mod checker;
mod typing;

fn main() {
	let file = File::new("app/main.baf".into());
	let mut lexer = Lexer::init(file);
	let mut parser = Parser::init(&mut lexer);
	Anaylzer::init(&mut parser).analyze();
}
