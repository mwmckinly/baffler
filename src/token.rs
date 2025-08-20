use serde::Serialize;

use crate::utilities::Printly;


#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Token {
  pub text: String,
  pub class: Class,
  pub spot: [usize; 2],
}


impl Token {
  pub fn new<T:ToString>(text: T, class: Class, spot: [usize; 2]) -> Token {
    return Token {
      text: text.to_string(),
      class: class,
      spot: spot,
    };
  }
}

impl std::fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      return write!(f, "{}", self.jsonify());
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Class {
  Identifier, Keyword,
  
  String, Number, Boolean,

  LBrace, RBrace,
  LBrack, RBrack,
  LParen, RParen,

  Dot, Comma, Colon, SemiColon,
  Comparator, Operator, Eof,

  Unknown, Logical, Arrow, Assign,
} 

impl std::fmt::Display for Class {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.jsonify())
  }
}