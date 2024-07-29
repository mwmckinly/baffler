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