use serde::Serialize;

use crate::blocks::{Argument, Expr, Node};
use crate::token::Token;


pub trait Printly {
  fn yamify(&self) -> String;
  fn jsonify(&self) -> String;
}

impl<T: Serialize> Printly for T {
  fn yamify(&self) -> String {
    serde_yaml::to_string(self).unwrap()
  }
  fn jsonify(&self) -> String {
    serde_json::to_string_pretty(self).unwrap()
  }
}


pub trait Coords {
  fn coords(&self) -> [usize; 2];
  fn length(&self) -> usize;
}

impl Coords for Token {
  fn coords(&self) -> [usize; 2] {
    return self.spot;
  }

  fn length(&self) -> usize {
    return self.text.len();
  }
}

impl Coords for Expr {
  fn coords(&self) -> [usize; 2] {
    match self {
      Expr::FunCall { name, .. } => name.coords(),
      Expr::Number { value }
        | Expr::String { value }
        | Expr::Variable { value }
        | Expr::Boolean { value } => value.coords(),
      Expr::Array { value } => {
          if value.is_empty() {
            [0, 0] // fallback if empty
          } else {
            value[0].coords()
          }
        }
      Expr::Index { from, .. } => from.coords(),
      Expr::BinaryOp { lhs, .. } => lhs.coords(),
      Expr::UnaryOp { expr, .. } => expr.coords(),
      Expr::Inline { args, code } => {
          if !args.is_empty() {
            args[0].coords()
          } else {
            code.coords()
          }
        }
      Expr::Unknown(prev) => {
          let [line, col] = prev.coords();
          [line, col + 1]
        }
      Expr::Type { name, .. } => name.coords(),
    }
  }

  fn length(&self) -> usize {
    match self {
      Expr::FunCall { name, args } => {
        let args_len: usize = args.iter().map(|arg| arg.length()).sum();
        let commas_len = if args.len() > 1 { args.len() - 1 } else { 0 };
        name.length() + 1 + args_len + commas_len + 1 
      }

      Expr::Number { value }
      | Expr::String { value }
      | Expr::Variable { value }
      | Expr::Boolean { value } => value.length(),

      Expr::Array { value } => {
        if value.is_empty() {
          2 // "[]"
        } else {
          let elems_len: usize = value.iter().map(|elem| elem.length()).sum();
          let commas_len = if value.len() > 1 { value.len() - 1 } else { 0 };
          1 + elems_len + commas_len + 1 // [ ... ]
        }
      }

      Expr::Index { from, index } => {
        from.length() + 1 + index.length() + 1 // a[b]
      }

      Expr::BinaryOp { lhs, op, rhs } => {
        lhs.length() + op.length() + rhs.length()
      }

      Expr::UnaryOp { expr, op } => {
        op.length() + expr.length()
      }

      Expr::Inline { args, code } => {
        // (args...) { code }
        let args_len: usize = args.iter().map(|arg| arg.length()).sum();
        let commas_len = if args.len() > 1 { args.len() - 1 } else { 0 };
        1 + args_len + commas_len + 1 + code.length()
      }

      Expr::Type { name, arrays } => {
        let start = name.length();
        return start + arrays * 2
      },

      Expr::Unknown(prev) => prev.length(),
    }
  }
}

impl Coords for Argument {
  fn coords(&self) -> [usize; 2] {
    self.0.coords()
  }

  fn length(&self) -> usize {
    let token_len = self.0.length();
    let expr_len = self.1.length();
    return token_len + expr_len;
  }
}

impl Coords for Node {
  fn coords(&self) -> [usize; 2] {
    match self {
      Node::SetAssign { name, .. } => name.coords(),
      Node::VarAssign { name, .. } => name.coords(),
      Node::ModifyVar { name, .. } => name.coords(),

      Node::FunDefine { name, .. } => name.coords(),

      Node::Condition { branches, fallback } => {
        if let Some((expr, _)) = branches.first() {
          expr.coords()
        } else if let Some(fallback) = fallback {
          fallback.coords()
        } else {
          [0, 0]
        }
      }

      Node::Compound { block } => {
        if let Some(first) = block.first() {
          first.coords()
        } else {
          [0, 0]
        }
      }

      Node::Emmission { value } => value.coords(),
      Node::ImportPkg { value } => value.coords(),

      Node::Expr(expr) => expr.coords(),
      Node::Unknown(expr) => expr.coords(),
    }
  }

  fn length(&self) -> usize {
    match self {
      Node::SetAssign { name, value }
      | Node::VarAssign { name, value }
      | Node::ModifyVar { name, value } => {
        name.length() + 1 + value.length()
      }

      Node::FunDefine { name, args, emit, node } => {
        let args_len: usize = args.iter().map(|arg| arg.length()).sum();
        let commas_len = if args.len() > 1 { args.len() - 1 } else { 0 };
        name.length() + 1 + args_len + commas_len + 1 + emit.length() + node.length()
      }

      Node::Condition { branches, fallback } => {
        let branches_len: usize = branches.iter()
          .map(|(expr, code)| expr.length() + code.length())
          .sum();
        let fallback_len = fallback.as_ref().map_or(0, |c| c.length());
        branches_len + fallback_len
      }

      Node::Compound { block } => block.iter().map(|n| n.length()).sum(),

      Node::Emmission { value } => value.length(),
      Node::ImportPkg { value } => value.length(),

      Node::Expr(expr) => expr.length(),
      Node::Unknown(expr) => expr.length(),
    }
  }
}

