use std::process::exit;

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