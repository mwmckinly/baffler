
use std::{collections::HashMap, fmt::Display};

use serde::Serialize;

use crate::{parser::Parser, syntax::{Expr, Node}, util::{Has, Color, SourceInfo}};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum IRType {
  String, 
  Number, 
  Boolean, 
  NullVoid,
  Array { of: Box<IRType> },
  Struct { fields: Vec<IRType> },
  None,
}
impl Display for IRType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().strip_suffix('\n').unwrap())
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum Symbol {
  Function { kind: IRType, args: Vec<IRType> },
  Variable { kind: IRType, mutable: bool },
  TypeRef { parent: IRType },
}
impl Symbol {
  pub fn new_func(kind: IRType, args: Vec<IRType>) -> Symbol {
    Self::Function { kind, args }
  }
  pub fn new_var(kind: IRType, mutable: bool) -> Symbol {
    Self::Variable { kind, mutable }
  }
  pub fn new_type(parent: IRType) -> Symbol {
    Self::TypeRef { parent }
  }
}
impl Display for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().strip_suffix('\n').unwrap())
  }
}

#[derive(Debug, Clone)]
struct Scope { 
  symbols: HashMap<String, Symbol>,
  parent: Option<Box<Scope>>
}
impl Scope {
  pub fn new(parent: Scope) -> Scope {
    Scope { symbols: HashMap::new(), parent: Some(Box::new(parent)) }
  }
  pub fn get<S:ToString>(&self, name: S) -> Option<&Symbol> {
    let symbol = self.symbols.get(&name.to_string());

    if symbol.is_some() {
      return symbol;
    }

    if self.parent.is_some() {
      return self.parent.as_ref().unwrap().get(name);
    }

    return None;
  }
  pub fn add<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.symbols.insert(name.to_string(), symbol);
  }
}

#[allow(non_snake_case)]
fn RootScope() -> Scope {
  let symbols = vec![
    ("str", IRType::String),
    ("num", IRType::Number),
    ("bool", IRType::Boolean),
    ("null", IRType::NullVoid)
  ].iter().map(|(name, kind)| {
    (name.to_string(), Symbol::new_type(kind.clone()))
  }).into_iter().collect();

  Scope { symbols, parent: None }
}

pub struct Anaylzer {
  source: Vec<Node>,
  scope: Scope,

  file: String,
  code: Vec<String>,
}

//: main / initialization functions
impl Anaylzer {
  pub fn init(parser: &mut Parser) -> Anaylzer {
    let root = RootScope();
    let file = parser.metadata();
    let tree = parser.parse();

    let tree = if let Node::Compound { body } = tree 
      { body } else { vec![tree] };

    Anaylzer {
      source: tree,
      scope: root,
      file: file.name(),
      code: file.lines(),
    }
  }
  pub fn analyze(&mut self) {
    for node in self.source.clone() {
      println!("{node}")
    }
  }
}

//: scope related functions
impl Anaylzer {
  fn enter(&mut self) {
    self.scope = Scope::new(self.scope.clone());
  }
  fn leave(&mut self) {
    if let Some(parent) = self.scope.parent.take() 
      { self.scope = *parent; } else { unreachable!() }
  }
  fn lookup<S:ToString>(&self, name: S) -> Option<&Symbol> {
    self.scope.get(name.to_string())
  }
  fn append<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.scope.add(name, symbol);
  }
}

impl Anaylzer {
  fn message<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, kind: String, header: V, message: S) {
    let (line, col, len) = node.info();

    let buffr = ' '.to_string().repeat(line.to_string().len());
    let space = ' '.to_string().repeat(col -1);
    let value = '~'.to_string().repeat(len - 1);

    let info = vec![
      format!("{}[{}:{}] --> {kind}: {header}", self.file, line, col),
      format!("{} | {}", line, self.source[line - 1].clone()),
      format!("{} | {}^{} {}", buffr, space, value, message.to_string())
    ].join("\n");

    println!("{info}")
  }
  fn error<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "error".color(31), header, message)
  }
  fn warn<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "warning".color(33), header, message)
  }
}

//: expression validation
impl Anaylzer {
  fn as_ir(&self, expr: &Expr) -> IRType { todo!() }
  fn check(&self, expr: &Expr) -> Result<(), ()> { todo!() }
}
