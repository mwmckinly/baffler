use core::fmt;

use serde::Serialize;
use crate::util::Color;

#[derive(Debug, PartialEq, Eq, Serialize, Clone, Copy)]
#[serde(rename_all = "kebab-case")]
pub enum Class {
  Identifier, Keyword,
  String, Number, 
  Boolean, Null,

  LeftBrace, RightBrace, 
  LeftBrack, RightBrack, 
  LeftParen, RightParen,

  Dot, Comma, Colon, SemiColon,
  BinaryOp, BooleanOp, LogicOp,
  Eof, Assign, Arrow, 
}

impl fmt::Display for Class {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().replace("\n", ""))
  }
}

#[derive(Debug, PartialEq, Eq, Serialize, Clone)]
pub struct Token {
  text: String,
  class: Class,
  index: [usize; 2]
}

impl Token {
  pub fn new<S:ToString>(class: Class, text: S, index: [usize; 2]) -> Token {
    Self { text: text.to_string(), class, index }
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "token:{}<text:'{}', index: {:?}>", self.class.color(33), self.text.color(32), self.index)
  }
}