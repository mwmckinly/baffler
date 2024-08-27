use crate::{syntax::{Expr, Node}, token::{Class, Token}};

pub trait Color {
  fn color(&self, code: u8) -> String;
}
impl<T:ToString> Color for T {
  fn color(&self, code: u8) -> String {
    format!("\x1b[{code}m{}\x1b0m", self.to_string())
  }
} 

pub trait Coords {
  fn coords(&self) -> [usize; 3];
}

pub trait Wrapper {
  fn wrap(self) -> Box<Self>;
}

impl<T> Wrapper for T {
  fn wrap(self) -> Box<Self> {
    return Box::new(self);
  }
}

impl Coords for [usize; 2] {
  fn coords(&self) -> [usize; 3] {
    [self[0], self[1], 1]
  }
}

impl Coords for &Token {
  fn coords(&self) -> [usize; 3] {
    let offset = if self.class == Class::String { 2 } else { 0 };
    [self.coords[0], self.coords[1], self.text.len() + offset]
  }
}
impl Coords for &Expr {
  fn coords(&self) -> [usize; 3] {
    match self {
      Expr::String { value } |
      Expr::Number { value } |
      Expr::Boolean { value } |
      Expr::VarRef { value } => value.coords(),
      Expr::FunCall { name, args } => {
        let [line, start, _] = name.coords();
        let [_, stop, last] = args.last().unwrap().coords();

        [line, start, stop - start + last + 1]
      },
      Expr::Array { value } => {
        if value.len() == 1 {
          return value[0].coords();
        }

        let [line, start, _] = value[0].coords();
        let [_, stop, last] = value.last().unwrap().coords();

        [line, start, stop - start + last]
      },
      Expr::Index { parent, index } => {
        let [line, start, _] = parent.coords();
        let [_, stop, last] = index.coords();

        [line, start, stop - start + last + 1]
      },
      Expr::Lambda { args, kind, .. } => {
        let [line, start, _] = args[0].coords();
        let [_, stop, last] = kind.coords();

        [line, start, stop - start + last]
      },
      Expr::IfExpr { cond, .. } => cond.coords(),
      Expr::BoolOper { lhs, rhs, .. } |
      Expr::MathOper { lhs, rhs, .. } |
      Expr::Chained { lhs, rhs, .. } => {
        let [line, start, _] = lhs.coords();
        let [_, stop, last] = rhs.coords();

        [line, start, stop - start + last]
      },
      Expr::TypeRef { base, arrs } => {
        let [line, start, length] = base.coords();
        [line, start, length + (2 * arrs)]
      }
      Expr::ObjectField { name, attr: kind } |
      Expr::TypePair { name, kind } => {
        let [line, start, _] = name.coords();
        let [_, stop, last] = kind.coords();

        [line, start, stop - start + last]
      },
      Expr::NullVoid { prev } => {
        let [line, start, length] = prev.coords();
        [line, start + length - 1, 1]
      },
      Expr::Object { attrs } => attrs[0].coords(),
    }
  }
}
impl Coords for &Node {
  fn coords(&self) -> [usize; 3] {
    match self {
      Node::SetAssign { name, value } |
      Node::VarAssign { name, value } => {
        let [line, start, _] = name.coords();
        let [_, stop, last] = value.coords();

        [line, start - 4, stop - start + last + 4]
      },
      Node::ChangeVal { name, value } => {
        let [line, start, _] = name.coords();
        let [_, stop, last] = value.coords();

        [line, start, stop - start + last]
      },
      Node::ImportLib { path } => {
        if path.len() == 1 {
          return path[0].coords();
        }

        let [line, start, _] = path[0].coords();
        let [_, stop, last] = path.last().unwrap().coords();

        [line, start - 4, stop - start + last + 5]
      },
      Node::EmitValue { value } => {
        let [line, start, len] = value.coords();

        [line, start - 5, len + 5]
      },
      Node::DeclareType { name, .. } => {
        let [line, first, len] = name.coords();
        [line, first - 5, len + 5]
      },
      Node::Compound { value } => value.first().unwrap().coords(),
      Node::Expression { expr } => expr.coords(),
    }
  }
}

impl<T:Coords> Coords for &[T] {
  fn coords(&self) -> [usize; 3] {
    if self.len() == 1 {
      return self[0].coords();
    }

    let [line, start, _] = self[0].coords();
    let [_, stop, last] = self.last().unwrap().coords();

    [line, start, stop - start + last]
  }
}

impl Coords for Token {
  fn coords(&self) -> [usize; 3] {
    return (&self).coords();
  }
}
impl Coords for Expr {
  fn coords(&self) -> [usize; 3] {
    return (&self).coords();
  }
}
impl Coords for Node {
  fn coords(&self) -> [usize; 3] {
    return (&self).coords();
  }
}
