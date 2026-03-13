use std::fmt::Display;




#[derive(Debug, Clone, Copy)]
pub enum LogLevel {
   Error,
   Warning,
   Info,
   Success,
}

impl Display for LogLevel {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      let (name, code) = match self {
         LogLevel::Error => ("error", 31),
         LogLevel::Warning => ("warning", 33),
         LogLevel::Info => ("info", 36),
         LogLevel::Success => ("success", 32),
      };

      return write!(f, "{}", prettify!(name, code));
   }
}


