use crate::{lexer::Lexer, tools::source::Source};



pub mod tools;
pub mod token;
pub mod lexer;
pub mod utils;



fn main() {
	let src = Source::from("app/main.baf".into());

	let (src, tokens) = Lexer::lex(src);

	tokens.into_iter().for_each(
		|tok| tok.disp(&src)
	);
}
