#![allow(unused)]


use std::{collections::HashMap, fmt};
use serde::Serialize;

use crate::token::Token;

type Value = Box<Expr>;
type Body = Box<Node>;

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
  String { value: Token },
  Number { value: Token },
  Boolean { value: Token },
  VarRef { value: Token },
  Object { kind: Token, args: Vec<Expr> },

  Array { items: Vec<Expr> },
  ArrItem { parent: Value, index: Value },
  MathOper { lhs: Value, rhs: Value, oper: Token },
  BoolOper { lhs: Value, rhs: Value, oper: Token },
  FunCall { name: Token, args: Vec<Expr> },
  Wrapper { expr: Value },

  Argument { name: Token, kind: Value },
  TypeRef { base: Token, arrays: usize },
  TypeCast { from: Value, to: Value },
  Attribute { parent: Value, field: Value },
  NullVoid,
}
impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expr::String { value } => write!(f, "expr:str<{:?}>", value.text),
      Expr::Number { value } => write!(f, "expr:num<{}>", value.text),
      Expr::Boolean { value } => write!(f, "expr:bool<{}>", value.text),
      Expr::VarRef { value } => write!(f, "expr:ident<{}>", value.text),
      Expr::Object { kind, .. } => write!(f, "expr:object<type: {}>", kind.text),
      Expr::Array { items } => write!(f, "expr:array<len: {}>", items.len()),
      Expr::ArrItem { parent, index } => write!(f, "expr:item<array: {}, index: {}>", parent, index),
      Expr::MathOper { lhs, rhs, oper } => write!(f, "expr:math-op<lhs: {lhs}, oper: {:?}, rhs: {rhs}>", oper.text),
      Expr::BoolOper { lhs, rhs, oper } => write!(f, "expr:bool-op<lhs: {lhs}, oper: {:?}, rhs: {rhs}>", oper.text),
      Expr::FunCall { name, args } => write!(f, "expr:fun-call<name: {}, args: {}>", name.text, args.len()),
      Expr::Wrapper { expr } => write!(f, "expr:wrapper<inner: {expr}>"),
      Expr::Argument { name, kind } => write!(f, "expr:arg<name: {}, type: {kind}>", name.text),
      Expr::TypeRef { base, arrays } => write!(f, "expr:type-ref<base: {}, arrays: {}>", base.text, arrays),
      Expr::TypeCast { from, to } => write!(f, "expr:case<from: {from}, to: {to}>"),
      Expr::Attribute { parent, field } => write!(f, "expr:attr<parent: {parent}, field: {field}>"),
      Expr::NullVoid => write!(f, "expr:null-void")
    }
  }
}


#[derive(Debug, Clone, Serialize)]
pub enum Node {
  SetAssign { name: Token, kind: Option<Expr>, value: Expr },
  VarAssign { name: Token, kind: Option<Expr>, value: Expr },
  ModifyVar { name: Token, value: Expr },

  FuncDefinition { name: Token, kind: Expr, args: Vec<Expr>, body: Body },
  DeclareObject { name: Token, fields: HashMap<Token, Expr> },
  ObjectField { field: Token, kind: Expr },
  ObjExtention { name: Token, body: Body },
  ValueEmission { expr: Expr },
  Compound { body: Vec<Node> },

  IfStatement { bool: Expr, body: Body, other: Body, },
  WhileLoop { bool: Expr, body: Body },

  ImportPackage { path: Vec<Token> },
  Expression { expr: Expr },
  NullVoid,
}
impl Node {
  pub fn name(&self) -> String {
    let name = match self {
      Node::SetAssign { .. } => "set-assign",
      Node::VarAssign { .. } => "var-assign",
      Node::ModifyVar { .. } => "modify-var",
      Node::FuncDefinition { .. } => "func-define",
      Node::DeclareObject { .. } => "declare-obj",
      Node::ObjectField { .. } => "obj-field",
      Node::ObjExtention { .. } => "obj-extention",
      Node::ValueEmission { .. } => "emit-value",
      Node::Compound { .. } => "compound",
      Node::IfStatement { .. } => "if-statement",
      Node::WhileLoop { .. } => "while-loop",
      Node::ImportPackage { .. } => "use-package",
      Node::Expression { .. } => "expression",
      Node::NullVoid => "null-void",
    };

    format!("node:{name}")
  }
}

impl fmt::Display for Node {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Node::SetAssign { name, kind, value } => {
        let kind = if kind.is_some() 
          { kind.clone().unwrap().to_string() } else { "None".to_string() };

        write!(f, "node:set<name: {}, value: {}, type?: {}>", name.text, value, kind)
      },
      Node::VarAssign { name, kind, value } => write!(f, "node:var<name: {}, value: {}, type?: {:?}>", name.text, value, kind),
      Node::ModifyVar { name, value } => write!(f, "node:modify<name: {}, value: {}>", name.text, value),
      Node::DeclareObject { name, fields } => write!(f, "node:object<name: {}, fields: {}>", name.text, fields.len()),
      Node::ObjectField { field, kind } => write!(f, "node:attr<name: {}, type: {}>", field.text, kind),
      Node::ObjExtention { name, body } => write!(f, "node:extention<name: {}, funcs: {}>", name.text, body),
      Node::FuncDefinition { name, kind, args, body } 
        => write!(f, "node:fun-def<name: {}, num-args: {}, type: {}, body: {}>", name.text, args.len(), kind, body),
      Node::ValueEmission { expr } => write!(f, "node:emit<value: {}>", expr),
      Node::Compound { body: nodes } => write!(f, "node:compound<nodes: {}>", nodes.len()),
      Node::IfStatement { bool, body, other } => write!(f, "node:if-node<bool: {bool}, if: {body}, else: {other}>"),
      Node::WhileLoop { bool, body } => write!(f, "node:while<bool: {bool}, body: {body}>"),
      Node::ImportPackage { path } => write!(f, "node:use<package: {}>", path.last().unwrap()),
      Node::Expression { expr: node } => write!(f, "node:{node}"),
      Node::NullVoid => write!(f, "node:null-void"),
    }
  }
}