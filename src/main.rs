#![allow(non_upper_case_globals)]

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

	println!("{}", file.name());
}
