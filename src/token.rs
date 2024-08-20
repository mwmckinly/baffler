use serde::Serialize;
use std::{fmt::{Debug, Display}, hash::Hash};

#[derive(Clone, Debug, Serialize, PartialEq, Eq)]
pub struct Token {
  pub class: Class,
  pub text: String,
  pub coords: [usize; 2]
}

impl Token {
  pub fn init<S:ToString>(class: Class, text: S, coords: [usize; 2]) -> Self {
    return Token { class, text: text.to_string(), coords, };
  }
}

impl Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "token:{} {{\n  text: {:?},\n  coords: {:?}\n}}", self.class, self.text, self.coords)
  }
}

impl Hash for Token {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.class.hash(state);
    self.text.hash(state);
  }
}

#[derive(Clone, Copy, Debug, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "kebab-case")]
pub enum Class {
  Identifier, Keyword,
  Bool, String, Number,

  LeftBrace, RightBrace,
  LeftBrack, RightBrack,
  LeftParen, RightParen,

  Arrow, Assign,

  Comma, SemiColon, Colon, Dot, 
  BoolOp, MathOp, LogicOp, Eof,
}

impl Display for Class {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let string = serde_yaml::to_string(self).unwrap();
    write!(f, "{}", string.strip_suffix("\n").unwrap())
  }
}