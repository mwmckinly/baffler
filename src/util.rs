use std::process::exit;

use crate::{syntax::{Expr, Node}, token::Token, visitor::{IRType, IRValue}};

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
      Expr::String { value } |
      Expr::Number { value } |
      Expr::Boolean { value } |
      Expr::VarRef { value } => value.info(),
      Expr::Lambda { args, body, .. } => {
        let start = args.last().unwrap().info();
        let stop = body.info();

        ( start.0, start.1 - 1, ( stop.2 + stop.1 - start.1 ) )
      },
      Expr::Array { items } => {
        let start = items.get(0).unwrap().info();
        let stop = items.last().unwrap().info();

        (start.0, start.1 - 1, (stop.2 + stop.1 - start.1) + 1)
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
        let stop = args.last().map_or(start, |arg| arg.info());

        ( start.0, start.1, ( stop.2 + stop.1 - start.1 ) + 1 )
      },
      Expr::Wrapper { expr } => {
        let stats = expr.info();

        ( stats.0, stats.1 - 1, stats.2 + 1 )
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


trait ToIR {
  fn ir_type(&self) -> IRType;
}

impl ToIR for Expr {
  fn ir_type(&self) -> IRType {
    match self {
      Expr::String { .. } => IRType::String,
      Expr::Number { .. } => IRType::Number,
      Expr::Boolean { .. } => IRType::Boolean,
      Expr::Lambda { emit, .. } => emit.ir_type(),
      Expr::Array { items } => IRType::Array { of: Box::new(items[0].ir_type()) },
      Expr::ArrItem { parent, .. } => {
        match parent.ir_type() {
          IRType::Array { of } => *of,
          IRType::String => IRType::String,
          _ => IRType::Null
        }
      },
      Expr::MathOper { lhs, .. } => lhs.ir_type(),
      Expr::BoolOper { .. } => IRType::Boolean,
      Expr::Wrapper { expr } => expr.ir_type(),
      Expr::Argument { kind, .. } => kind.ir_type(),
      Expr::TypeRef { base, arrays } => {
        let mut kind: IRType = match base.text.as_str() {
          "str" => IRType::String,
          "num" => IRType::Number,
          "null" => IRType::Null,
          "bool" => IRType::Boolean,
          _ => IRType::Null,
        };

        for _ in 0..*arrays { kind = IRType::Array { of: Box::new(kind) } }

        kind
      },
      Expr::TypeCast { to, .. } => to.ir_type(),
      _ => IRType::Null
    }
  }
}


