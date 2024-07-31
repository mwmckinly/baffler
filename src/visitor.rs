use std::{collections::HashMap, fmt::Display};

use crate::{parser::Parser, syntax::{Expr, Node}, token::Token, util::{Color, SourceInfo}};

#[derive(Clone, PartialEq, Debug)]
pub enum IRType {
  String, 
  Number, 
  Boolean,
  Array { of: Box<IRType> },
  Null,
Any,
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
  Function { args: HashMap<String, IRType>, kind: IRType, body: Box<IRNode> },
  Variable { value: IRValue, mutable: bool },
}

#[derive(Debug, Clone)]
pub struct Scope {
  symbols: HashMap<String, Symbol>,
  parent: Option<usize>
}

impl Scope {
  pub fn new(parent: Option<usize>) -> Self {
    return Scope { symbols: HashMap::new(), parent, };
  }
}

pub struct Analyzer {
  program: Vec<Node>,
  pointer: usize,
  
  scopes: Vec<Scope>,
  current: usize,

  filename: String,
  source: Vec<String>,
}

impl Analyzer {
  pub fn init(parser: &mut Parser) -> Self {
    let program = if let Node::Compound { body } = parser.parse() { body } else { unreachable!() };

    let file = parser.metadata();

    Analyzer {
      program, pointer: 0,
      scopes: vec![Scope::new(None)],
      current: 0, filename: file.name(),
      source: file.lines(),
    }
  }

  fn scope(&mut self) -> &mut Scope {
    self.scopes.get_mut(self.current).unwrap()
  }

  pub fn lookup<S:ToString>(&mut self, var: S) -> Option<&Symbol> {
    return self.scope().symbols.get(&var.to_string()).clone();
  }

  fn error<V:Display, S:ToString>(&self, node: &Node, header: V, message: S) {
    let (line, col, len) = node.info();

    let buffr = ' '.to_string().repeat(line.to_string().len());
    let space = ' '.to_string().repeat(col -1);
    let value = '~'.to_string().repeat(len - 1);

    let info = vec![
      format!("{}[{}:{}] --> {}: {header}", self.filename, line, col, "error".color(31)),
      format!("{} | {}", line, self.source[line - 1].clone()),
      format!("{} | {}^{} {}", buffr, space, value, message.to_string())
    ].join("\n");

    println!("{info}")
  }

  fn check_node(&mut self, node: &Node) {
    match node {
      Node::SetAssign { .. } => self.check_assignment(node),
      Node::VarAssign { .. } => self.check_assignment(node),
      Node::ModifyVar { name, value } => todo!(),
      Node::FuncDefinition { name, kind, args, body } => todo!(),
      Node::ValueEmission { expr } => todo!(),

      _ => (),
    }
  }
  fn check_assignment(&mut self, node: &Node) {
    let (name, kind, value, mutable) = if let
      Node::SetAssign { name, kind, value } = node 
        { ( name.text.clone(), kind, value, false ) } else if let 
      Node::VarAssign { name, kind, value } = node
        { ( name.text.clone(), kind, value, true ) } else 
        { unreachable!() };

    if self.lookup(&name).is_some() {
      self.error(node, "symbol already exists", format!("cannot assign value to {name:?}, it has already been assigned."));
    }

    let var_kind = if kind.is_some() 
      { self.get_ir_type(kind) } else { IRType::Any };

    let val_kind = self.get_ir_type(value);

    if kind.is_some() && var_kind != self.get_ir_type(value) {
      self.error(node, "invalid assignment", format!("cannot assign value of type {val_kind} to {name:?} ({var_kind}).", ));
    }

    self.scope().symbols.insert(name.clone(), Symbol::Variable { value: self.get_ir_value(value), mutable });
  }
  fn check_modification(&mut self, node: &Node) {
    let (name, value) = if let Node::ModifyVar { name, value } = node {
      (name.text.clone(), value)
    } else { unreachable!() };

    let var = self.lookup(&name);
    
    let var = if var.is_none() {
      self.error(node, "symbol does not exist", format!("{name} has never been initialized")); panic!()
    } else { var.unwrap() };

    let ( vv, mutable ) = if let Symbol::Variable { value, mutable } = var {
      ( value, mutable )
    } else { unreachable!() };

    if !mutable {
      self.error(node, "attempted to reassign constant", format!("{name} was declared as a constant and cannot be modified."))
    }

    if vv.ir_type() != value.ir_type() {
      self.error(node, "invalid type", format!("{name} is a {}, cannot assign a value of type {} to {}", value.ir_type(), vv.ir_type(), value.ir_type()))
    }

    
  }
}

//fn message<S:Display, V:Display>(&self, token: &Token, header: V, error: S) {
//   let buffr = ' '.to_string().repeat(token.index[0].to_string().len());
//   let space = ' '.to_string().repeat(token.index[1] -1);
//   let value = '~'.to_string().repeat(token.text.len() - 1);
//   let area = format!("{}[{}:{}]", self.filename, token.index[1], token.index[0]);

//   let prob = vec![
//     format!("{area} <- {header}: unexpected {}", token.class),
//     format!("{} | {}", token.index[0], self.source[token.index[0] - 1]),
//     format!("{} | {}^{} {}", buffr, space, value, error.to_string())
//   ].join("\n");

//   println!("{prob}")
// }
