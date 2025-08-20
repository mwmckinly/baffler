use crate::{logger::{LogLevel, Logger}, token::{Class, Token}};

pub struct Lexer {
  index: usize,
  source: Vec<char>,
  logger: Logger,
  tokens: Vec<Token>,
  line: usize, col: usize,
}

impl Lexer {
  pub fn initialize(filename: String, source: String) -> Lexer {
    let logger = Logger::new(filename, source.clone());

    return Lexer { 
      index: 0, 
      source: source.chars().collect(), 
      logger, 
      tokens: vec![] , line: 1, col: 1,
    };
  }

  fn cur(&self) -> char {
    return *self.source.get(self.index).unwrap_or(&'\0');
  }

  fn walk(&mut self) {
    self.index += 1;
    self.col += 1;
  }

  fn to_token(&self, text: char, class: Class) -> Token {
    return Token::new(text.to_string(), class, [self.line, self.col]);
  }

  fn append_token(&mut self, class: Class) {
    self.tokens.push(self.to_token(self.cur(), class));
    self.walk();
  }

  fn make_token(&mut self, text: String, class: Class, spot: [usize; 2]) {
    self.tokens.push(Token::new(text, class, spot));
  }

  fn next_token(&mut self) {
    let cur = self.cur();
    let start = [self.line, self.col];

    match cur {
      _ if cur.is_alphabetic() || cur == '_' => {
        let mut text = "".to_string();

        while self.cur().is_alphanumeric() || cur == '_' {
          text.push(self.cur());
          self.walk();
        }

        let class = match text.as_str() {
          "fun" | "set" | "var" |
          "emit" | "if" | "else" |
          "import" => Class::Keyword,

          "true" | "false" => Class::Boolean,
          
          _ => Class::Identifier
        };

        self.make_token(text, class, start);
      },
      _ if cur.is_numeric() => {
        let mut buffer = String::new();
        while self.cur().is_numeric() {
          buffer.push(self.cur());
          self.walk();
        }

        self.make_token(buffer, Class::Number, start);
      },

      '"' => {
        let mut text = "".to_string();
        self.walk();

        while self.cur() != '"' {
          if self.cur() == '\\' {
            self.walk();
            text.push_str(&format!("\\{}", self.cur()));
            self.walk();
          } else {
            text.push(self.cur());
            self.walk();
          }
        }

        self.walk();
        self.make_token(text, Class::String, start);
      },

      '[' => self.append_token(Class::LBrace),
      '{' => self.append_token(Class::LBrack),
      '(' => self.append_token(Class::LParen),

      ']' => self.append_token(Class::RBrace),
      '}' => self.append_token(Class::RBrack),      
      ')' => self.append_token(Class::RParen),

      '.' => self.append_token(Class::Dot),
      ',' => self.append_token(Class::Comma),
      ':' => self.append_token(Class::Colon),
      ';' => self.append_token(Class::SemiColon),

      '+' | '*' | '/' | '%' => {
        self.walk();
        let val = if self.cur() == '=' {
          self.walk();
          format!("{cur}=")
        } else { cur.to_string() };

        self.make_token(val, Class::Operator, start);
      },

      '|' | '&' => {
        self.append_token(Class::Logical);
      },

      '!' => {
        self.walk();
        let text = if self.cur() == '=' { self.walk(); "!=" } else { "!" };
        self.make_token(text.into(), Class::Comparator, start);
      }

      '=' => {
        self.walk();
        let (text, class) = match self.cur() {
          '=' => { self.walk(); ("==", Class::Comparator) },
          '>' => { self.walk(); ("=>", Class::Arrow) },
          _ => ("=", Class::Assign)
        };

        self.make_token(text.into(), class, start);
      },

      '-' => {
        self.walk();
        let text = if self.cur() == '=' { self.walk(); "-=" } else { "-" };
        self.make_token(text.into(), Class::Operator, start);
      },

      ' ' => self.walk(),
      '\n' => { self.walk(); self.line += 1; self.col = 1; },

      '#' => {
        while self.cur() != '\n' && self.cur() != '\0' { self.walk(); }
      },

      _ => {
        let bad_char = self.to_token(cur, Class::Unknown);
        self.logger.log(LogLevel::Error, &bad_char, format!("{cur:?} is not a valid character"));
        self.walk();
      },
    };
  }

  pub fn tokenize(mut self) -> (Vec<Token>, Logger) {
    while self.index <= self.source.len() - 1{
      self.next_token();
    }

    self.tokens.push(Token::new("\0", Class::Eof, [self.line, self.col]));

    let errors = self.logger.num_errors();

    if errors > 0 {
      println!("[LEXER]: Could not proceed to parsing due to {errors} prevous errors.", );
      std::process::exit(1);
    }

    return (self.tokens, self.logger);
  }
}