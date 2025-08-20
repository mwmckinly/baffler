use std::fmt::Display;
use serde::Serialize;
use crate::{token::Token, utilities::Printly};

#[derive(Debug, Serialize)]
pub struct Argument(pub Token, pub Expr);
type Code = Box<Node>;

#[derive(Debug, Serialize)]
pub enum Node {
  SetAssign { name: Token, value: Expr },
  VarAssign { name: Token, value: Expr },
  ModifyVar { name: Token, value: Expr },

  FunDefine {
    name: Token,
    args: Vec<Argument>,
    emit: Expr,
    node: Code,
  },

  Condition {
    branches: Vec<(Expr, Code)>,
    fallback: Option<Code>
  },

  Compound { block: Vec<Node> },

  Emmission { value: Expr },
  ImportPkg { value: Expr },

  Expr(Expr),
  Unknown(Expr),
}

#[derive(Debug, Serialize)]
pub enum Expr {
  FunCall { name: Token, args: Vec<Expr> },

  Number { value: Token },
  String { value: Token },
  Variable { value: Token },
  Boolean { value: Token },

  Array { value: Vec<Expr> },
  Index { from: Box<Expr>, index: Box<Expr> },

  BinaryOp { lhs: Box<Expr>, op: Token, rhs: Box<Expr> },
  UnaryOp { expr: Box<Expr>, op: Token },

  Inline { args: Vec<Expr>, code: Code },

  Type { name: Token, arrays: usize },
  Unknown(Box<Expr>),
}

impl Display for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{}", self.jsonify());
  }
}

impl Display for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    return write!(f, "{}", self.jsonify());
  }
}

