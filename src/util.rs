use std::process::exit;

use crate::syntax::{Expr, Node};
use crate::token::{Class, Token}; 

pub trait Color {
  fn color(&self, f: usize) -> String;
}
impl<T:ToString> Color for T {
  fn color(&self, f: usize) -> String {
    format!("\x1b[{f}m{}\x1b[0m", self.to_string())
  }
}

pub trait Has<T> {
  fn has(&self, item: &T) -> bool;
}
impl<T:PartialEq> Has<T> for [T] {
  fn has(&self, item: &T) -> bool {
    self.iter().any(|x| x == item)
  }
}

#[derive(Debug, Clone)]
pub struct File {
  name: String, text: String,
}

impl File {
  pub fn new(name: String) -> File {
    let text = if let Ok(text) = std::fs::read_to_string(&name) 
      { text } else { println!("{}: invalid filename, {name} doesnt exist.", "error".color(31)); exit(1) };

    File { name, text }
  }
  pub fn init(name: String, text: String) -> File {
    File { name, text }
  }

  pub fn lines(&self) -> Vec<String> {
    self.text.split("\n").map(|x| x.to_string()).collect()
  }
  pub fn chars(&self) -> Vec<char> {
    self.text.chars().collect()
  }
  pub fn name(&self) -> String {
    self.name.clone()
  }
}

pub trait SourceInfo {
  fn info(&self) -> (usize, usize, usize);
}

impl SourceInfo for Token {
  fn info(&self) -> (usize, usize, usize) {
    ( self.index[0], self.index[1], self.text.len() )
  }
}
impl SourceInfo for Expr {
  fn info(&self) -> (usize, usize, usize) {
    match self {
      Expr::String { value } => {
        let base = value.info();
        ( base.0, base.1, base.2 + 2 )
      },
      Expr::Number { value } |
      Expr::Boolean { value } |
      Expr::VarRef { value } => value.info(),
      Expr::Lambda { args, .. } => {
        let start = args.first().unwrap().info();
        let stop = args.last().unwrap().info();

        ( start.0, start.1 - 1, ( stop.2 + stop.1 - start.1 ) + 2)
      },
      Expr::Object { kind, args } => {
        let start = kind.info();
        let stop = args.last().map_or(start, |arg| arg.info());

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) + 2 )
      },
      Expr::Array { items } => {
        let start = items.get(0).unwrap().info();
        let stop = items.last().unwrap().info();

        (start.0, start.1 - 1, (stop.2 + stop.1 - start.1) + 2)
      },
      Expr::ArrItem { parent, index } => {
        let start = parent.info();
        let stop = index.info();

        ( start.0, start.1, (stop.2 + stop.1 - start.1) + 1 )
      },
      Expr::MathOper { lhs, rhs, .. } |
      Expr::BoolOper { lhs, rhs, .. } => {
        let start = lhs.info();
        let stop = rhs.info();

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) )
      },
      Expr::FunCall { name, args } => {
        let start = name.info();
        let stop = args.last().map_or(
          (start.0, start.1 + 2, 3), 
          |arg| arg.info()
        );

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) + 1 )
      },
      Expr::Wrapper { expr } => {
        let stats = expr.info();

        ( stats.0, stats.1 - 1, stats.2 + 2 )
      },
      Expr::Argument { name, kind } => {
        let start = name.info();
        let stop = kind.info();

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) )
      },
      Expr::TypeRef { base, arrays } => {
        let start = base.info();
        let mut size = 0;

        for _ in 0..*arrays { size += 2; }
        ( start.0, start.1, start.2 + size )
      },
      Expr::TypeCast { from, to } => {
        let start = from.info();
        let stop = to.info();

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) + 4 )
      },
      Expr::Attribute { parent, field } => {
        let start = parent.info();
        let stop = field.info();

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) + 1 )
      },
      Expr::NullVoid => todo!(),
    }
  }
}

impl SourceInfo for Node {
  fn info(&self) -> (usize, usize, usize) {
    match self {
      Node::SetAssign { name, value, .. }
      | Node::VarAssign { name, value, .. } => {
          let (start_line, start_col, _) = name.info();
          let (_, end_col, end_len) = value.info();
          (start_line, start_col, end_col + end_len - start_col)
      }

      Node::ModifyVar { name, value } => {
          let (start_line, start_col, _) = name.info();
          let (_, end_col, end_len) = value.info();
          (start_line, start_col, end_col + end_len - start_col)
      }

      Node::FuncDefinition { name, body, .. } => {
          let (start_line, start_col, _) = name.info();
          let (_, end_col, end_len) = body.info();
          (start_line, start_col, end_col + end_len - start_col)
      }

      Node::ValueEmission { expr }
      | Node::Expression { expr } => expr.info(),

      Node::ImportPackage { path } => {
          let (start_line, start_col, _) = path.first().map_or((0, 0, 0), |t| t.info());
          let (_, end_col, end_len) = path.last().map_or((0, 0, 0), |t| t.info());
          (start_line, start_col, end_col + end_len - start_col)
      }

      _ => (0, 0, 0),
    }
  }
}

pub struct Location {
  line: usize,
  column: usize,
  length: usize,
}

pub trait GetArea {
  fn area(&self) -> Location;
}

impl GetArea for Token {
  fn area(&self) -> Location {
    let modifier = if self.class == Class::String 
      { 2 } else { 0 };
    Location { line: self.index[0], column: self.index[1], length: self.text.len() + modifier }
  }
}

impl GetArea for Expr {
  fn area(&self) -> Location {
    let modifier: usize = match self {
      Expr::Lambda { ..} |
      Expr::Array { .. } |
      Expr::ArrItem { .. } |
      Expr::FunCall { .. } |
      Expr::Wrapper { .. } => 2,
      Expr::Argument { .. } |
      Expr::Attribute { .. } => 1,
      _ => 0
    };

    match self {
      Expr::String { value } |
      Expr::Number { value } |
      Expr::Boolean { value } |
      Expr::VarRef { value } => value.area(),
      Expr::Lambda { args, emit, body } => todo!(),
      Expr::Object { kind, args } => todo!(),
      Expr::Array { items } => todo!(),
      Expr::ArrItem { parent, index } => todo!(),
      Expr::MathOper { lhs, rhs, oper } => todo!(),
      Expr::BoolOper { lhs, rhs, oper } => todo!(),
      Expr::FunCall { name, args } => todo!(),
      Expr::Wrapper { expr } => todo!(),
      Expr::Argument { name, kind } => todo!(),
      Expr::TypeRef { base, arrays } => todo!(),
      Expr::TypeCast { from, to } => todo!(),
      Expr::Attribute { parent, field } => todo!(),
      Expr::NullVoid => todo!(),
    }
  }
}