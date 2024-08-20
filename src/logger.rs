use crate::utils::Coords;

#[derive(Debug, Clone)]
pub struct Logger {
  filename: String,
  source: Vec<String>,
}

impl Logger {
  pub fn new(filename: String, source: String) -> Self {
    let source = source.lines().into_iter().map(|x| x.into()).collect::<Vec<String>>();
    Self { filename, source, }
  }

  pub fn get_chars(&self) -> Vec<char> {
    return (self.source.join("\n") + "\0").chars().collect();
  }

  fn message(&self, kind: &str, header: String, message: String, info: [usize; 3]) -> String {
    let (line, col, len) = (info[0], info[1], info[2]);

    let buffr = ' '.to_string().repeat(line.to_string().len());
    let space = ' '.to_string().repeat(col -1);
    let value = '~'.to_string().repeat(len - 1);

    let info = vec![
      format!("{kind} -> {}[{}:{}]: {header}", self.filename, line, col),
      format!("{} | {}", line, self.source[line - 1].clone()),
      format!("{} | {}^{} {}", buffr, space, value, message.to_string())
    ].join("\n");

    return info;
  }
  pub fn error<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) -> String {
    return self.message("error", header.to_string(), message.to_string(), spot.coords());
  }
  pub fn inform<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) -> String {
    return self.message("info", header.to_string(), message.to_string(), spot.coords());
  }
  pub fn warn<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) -> String {
    return self.message("warning", header.to_string(), message.to_string(), spot.coords());
  }
}