use serde::Serialize;
use std::fmt::Debug;

use crate::token::Token;

type Value = Box<Expr>;
type Body = Box<Node>;

#[derive(Clone, Serialize)]
pub enum Node {
  SetAssign { name: Token, value: Expr },
  VarAssign { name: Token, value: Expr },
  ChangeVal { name: Token, value: Expr },

  ImportLib { path: Vec<Token> },
  EmitValue { value: Expr },

  DeclareType { name: Token, attrs: Vec<Expr> },

  Compound { value: Vec<Node> },
  Expression{ expr: Expr },
}

#[derive(Clone, Serialize)]
pub enum Expr {
  String { value: Token },
  Number { value: Token },
  Boolean { value: Token },
  VarRef { value: Token },
  FunCall { name: Token, args: Vec<Expr> },

  Object { attrs: Vec<Expr> },
  ObjectField { name: Token, attr: Box<Expr> },
  Attribute { parent: Value, attr: Value },

  Array { value: Vec<Expr> },
  Index { parent: Value, index: Value },
  Lambda { args: Vec<Expr>, kind: Value, body: Body },
  IfExpr { cond: Value, body: Body, other: Body },

  BoolOper { lhs: Value, oper: Token, rhs: Value },
  MathOper { lhs: Value, oper: Token, rhs: Value },
  Chained { lhs: Value, stich: Token, rhs: Value },

  TypeRef { base: Token, arrs: usize, },
  TypePair { name: Token, kind: Value },
  NullVoid { prev: Token },
}

impl Debug for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let string = serde_yaml::to_string(self).unwrap();
    write!(f, "{}", string.strip_suffix("\n").unwrap())
  }
}
impl Debug for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let string = serde_yaml::to_string(self).unwrap();
    write!(f, "{}", string.strip_suffix("\n").unwrap())
  }
}