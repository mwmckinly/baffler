use std::collections::HashMap;

use crate::{parser::Parser, syntax::{Expr, Node}, token::Token, util::Color};

#[derive(Clone, PartialEq, Debug)]
pub enum IRType {
  String, 
  Number, 
  Boolean,
  Array { of: Box<IRType> },
  Null
}

#[derive(Clone, PartialEq, Debug)]
pub enum IRValue {
  String { value: Vec<char> },
  Number { value: f64 },
  Boolean { value: bool },
  Array { value: Vec<IRValue> },
  Null
}

#[derive(Clone, PartialEq, Debug)]
pub enum IRNode {
  AssignVariable { name: String, value: IRValue, mutable: bool },
  CallFunction { name: String, args: Vec<IRValue> },
  DeclareFunction { name: String, args: HashMap<String, IRType>, kind: IRType, body: Box<IRNode> },

  EvaluateBinaryExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 0: +, 1: -, 2: *, 3: /, 4: %
  EvaluateBooleanExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 10: ==, 11: <, 12: >, 13: <=, 14: >=, 15: !=

  ReturnValue { value: IRValue },
  Compound { body: Vec<IRNode> },
}

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
  Function { name: String, args: HashMap<String, IRType>, kind: IRType, body: Box<IRNode> },
  Variable { name: String, value: IRValue, mutable: bool },
}

#[derive(Debug, Clone)]
pub struct Scope {
  symbols: HashMap<usize, Symbol>,
  parent: Option<Box<Scope>>
}

impl Scope {
  pub fn new(parent: Option<Box<Scope>>) -> Self {
    return Scope { symbols: HashMap::new(), parent, };
  }
}

pub struct Analyzer {
  program: Node,
  pointer: usize,
  
  scopes: Vec<Scope>,
  current: usize,

  filename: String,
  source: Vec<String>,
}