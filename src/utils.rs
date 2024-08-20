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