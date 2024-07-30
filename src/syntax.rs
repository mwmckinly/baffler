use std::fmt;
use serde::Serialize;

use crate::token::Token;

type Value = Box<Expr>;
type Body = Box<Node>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Expr {
  String { value: Token },
  Number { value: Token },
  Boolean { value: Token },
  VarRef { value: Token },
  Lambda { args: Vec<Expr>, emit: Value, body: Body },

  Array { items: Vec<Expr> },
  ArrItem { parent: Value, index: Value },
  MathOper { lhs: Value, rhs: Value, oper: Token },
  BoolOper { lhs: Value, rhs: Value, oper: Token },
  FunCall { name: Token, args: Vec<Expr> },

  Argument { name: Token, kind: Value },
  TypeRef { base: Token, arrays: usize },
  TypeCast { from: Value, to: Value },
  Attribute { parent: Value, field: Value },
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::String { value } => write!(f, "expr:str<{:?}>", value.text),
      Expr::Number { value } => write!(f, "expr:num<{}>", value.text),
      Expr::Boolean { value } => write!(f, "expr:bool<{}>", value.text),
      Expr::VarRef { value } => write!(f, "expr:ident<{}>", value.text),
      Expr::Lambda { args, emit, body } => write!(f, "expr:lambda<args: {}, type: {emit}, body: {body}>", args.len()),
      Expr::Array { items } => write!(f, "expr:array<len: {}>", items.len()),
      Expr::ArrItem { parent, index } => write!(f, "expr:item<array: {}, index: {}>", parent, index),
      Expr::MathOper { lhs, rhs, oper } => write!(f, "expr:math-op<lhs: {lhs}, oper: {:?}, rhs: {rhs}>", oper.text),
      Expr::BoolOper { lhs, rhs, oper } => write!(f, "expr:bool-op<lhs: {lhs}, oper: {:?}, rhs: {rhs}>", oper.text),
      Expr::FunCall { name, args } => write!(f, "expr:fun-call<name: {}, args: {}>", name.text, args.len()),
      Expr::Argument { name, kind } => write!(f, "expr:arg<name: {}, type: {kind}>", name.text),
      Expr::TypeRef { base, arrays } => write!(f, "expr:type-ref<base: {}, arrays: {arrays}>", base.text),
      Expr::TypeCast { from, to } => write!(f, "expr:case<from: {from}, to: {to}>"),
      Expr::Attribute { parent, field } => write!(f, "expr:attr<parent: {parent}, field: {field}>"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Node {
  SetAssign { name: Token, kind: Option<Expr>, value: Expr },
  VarAssign { name: Token, kind: Option<Expr>, value: Expr },
  ModifyVar { name: Token, value: Expr },

  FuncDefinition { name: Token, kind: Expr, args: Vec<Expr>, body: Body },
  ValueEmission { expr: Expr },
  Compound { nodes: Vec<Node> },

  IfStatement { bool: Expr, body: Body, other: Body, },
  WhileLoop { bool: Expr, body: Body },

  ImportPackage { path: Vec<Token> },
  Expression { node: Expr },
  NullVoid,
}

impl fmt::Display for Node {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Node::SetAssign { name, kind, value } => write!(f, "node:set<name: {}, value: {}, type?: {:?}>", name.text, value, kind),
      Node::VarAssign { name, kind, value } => write!(f, "node:var<name: {}, value: {}, type?: {:?}>", name.text, value, kind),
      Node::ModifyVar { name, value } => write!(f, "node:modify<name: {}, value: {}>", name.text, value),
      Node::FuncDefinition { name, kind, args, body } 
        => write!(f, "node:fun-def<name: {}, num-args: {}, type: {}, body: {}>", name.text, args.len(), kind, body),
      Node::ValueEmission { expr } => write!(f, "node:emit<value: {}>", expr),
      Node::Compound { nodes } => write!(f, "node:compound<nodes: {}>", nodes.len()),
      Node::IfStatement { bool, body, other } => write!(f, "node:if-node<bool: {bool}, if: {body}, else: {other}>"),
      Node::WhileLoop { bool, body } => write!(f, "node:while<bool: {bool}, body: {body}>"),
      Node::ImportPackage { path } => write!(f, "node:use<package: {}>", path.last().unwrap()),
      Node::Expression { node } => write!(f, "node:{node}"),
      Node::NullVoid => write!(f, "node:null-void"),
    }
  }
}