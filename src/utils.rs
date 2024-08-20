use crate::{syntax::{Expr, Node}, token::Token};

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
    [self.coords[0], self.coords[1], self.text.len()]
  }
}

impl Coords for &Expr {
  fn coords(&self) -> [usize; 3] {
    todo!()
  }
}
impl Coords for &Node {
  fn coords(&self) -> [usize; 3] {
    todo!()
  }
}