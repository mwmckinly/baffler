use crate::utilities::{Coords, Printly};

pub struct Logger {
  filename: String,
  source: String,
  errors: usize,
}

#[derive(serde::Serialize, PartialEq, Eq)]
#[serde(rename_all = "UPPERCASE")]
pub enum LogLevel {
  Error, Warning, Success, Info
}

impl std::fmt::Display for LogLevel {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let color: &str = match self {
      LogLevel::Error => "\x1b[31m",
      LogLevel::Warning => "\x1b[33m",
      LogLevel::Success => "\x1b[32m",
      LogLevel::Info => "\x1b[36m",
    };

    write!(f, "{color}{}\x1b[0m", self.jsonify().replace("\"", ""))
  }
}

impl Logger {
  pub fn new(filename: String, source: String) -> Self {
    return Logger { filename, source, errors: 0 };
  }

  pub fn log<T:Coords, S:ToString>(&mut self, level: LogLevel, node: &T, info: S) {
    let coords = node.coords();

    let line = self.source.lines().nth(coords[0] - 1).unwrap();
    let line_buf = " ".repeat(coords[0].to_string().len());
    let spacing = " ".repeat(coords[1] - 1);
    let underline = "~".repeat(node.length() - 1);

    if level == LogLevel::Error { self.errors += 1; }

    println!("{}", [
      format!("[{}] --> {}{:?}", level, self.filename, coords),
      format!(" {} | {line}", coords[0]),
      format!(" {} | {spacing}^{underline} {}", line_buf, info.to_string())
    ].join("\n"));
  }

  pub fn num_errors(&self) -> usize {
    return self.errors;
  }
}

