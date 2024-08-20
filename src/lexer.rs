use crate::{logger::Logger, token::{Class, Token}, utils::Wrapper};

pub struct Lexer {
  tokens: Vec<Token>,
  logger: Box<Logger>,
  source: Vec<char>,
  coords: [usize; 2],
  pointer: usize,
}

impl Lexer {
  pub fn new(logger: Logger) -> Self {
    let lexer = Lexer {
      tokens: vec![],
      source: logger.get_chars(),
      logger: logger.wrap(),
      coords: [1, 1],
      pointer: 0,
    };

    return lexer;
  }
  pub fn tokenize(mut self) -> Vec<Token> {
    while self.pointer < self.source.len() 
      { self.get_next(); }
    
    return self.tokens;
  }

  fn advance(&mut self) {
    self.pointer += 1;
    self.coords[1] += 1;
  }
  fn current(&self) -> char {
    return self.source[self.pointer];
  }
  fn append<S:ToString>(&mut self, text: S, class: Class, coords: [usize; 2]) {
    self.tokens.push(Token::init(class, text, coords));
  }
  fn push_c(&mut self, class: Class) {
    self.append(self.current(), class, self.coords);
    self.advance();
  }
}

impl Lexer {
  fn get_next(&mut self) {
    let char = self.current();
    let coords = self.coords;

    match char {
      _ if char.is_ascii_alphabetic() => {
        let mut text = String::new();
        while self.current().is_ascii_alphanumeric() || self.current() == '_' {
          text.push(self.current()); self.advance();
        }

        let class = match text.as_str() {
          "true" | "false" => Class::Bool,

          "use" | "set" | "var" |
          "emit" | "fun" => Class::Keyword,
          
          _ => Class::Identifier,
        };

        self.append(text, class, coords);
      },
      _ if char.is_ascii_digit() => {
        let mut text = String::new();
        let mut dots = 0;

        loop {
          match self.current() {
            '.' => { dots += 1; text += "."; self.advance(); },
            '0'..'9' => { text.push(self.current()); self.advance(); },

            _ => break,
          }

          if dots == 2 { break; }
        }

        self.append(text, Class::Number, coords);
      },
      '"' => {
        let mut text = String::new(); self.advance();

        loop {
          match self.current() {
            '"' => { self.advance(); break; },
            '\\' => {
              self.advance(); text += "\\";
              text.push(self.current()); self.advance();
            },

            _ => { text.push(self.current()); self.advance(); },
          }
        }

        self.append(text, Class::String, coords)
      },

      '+' | '*' | '/' | '%' => {
        self.advance();
        if self.current() == '=' {
          self.append(char.to_string() + "=", Class::MathOp, coords);
          self.advance();
        } else {
          self.append(char, Class::MathOp, coords);
        };
      },

      '=' => {
        self.advance();
        match self.current() {
          '=' => self.append("==", Class::BoolOp, coords),
          _ => self.append("=", Class::Assign, coords),
        }
      },

      '-' => {
        self.advance();
        match self.current() {
          '=' => { self.advance(); self.append("-=", Class::MathOp, coords) },
          '>' => { self.advance(); self.append("->", Class::Arrow, coords) },
          _ => self.append("-", Class::MathOp, coords)
        }
      },

      '!' | '<' | '>' => {
        self.advance();
        if self.current() == '=' {
          self.append(char.to_string() + "=", Class::BoolOp, coords);
          self.advance();
        } else {
          self.append(char, Class::BoolOp, coords);
        };
      },

      '|' | '&' | '^' => self.push_c(Class::LogicOp),

      '[' => self.push_c(Class::LeftBrace),
      '{' => self.push_c(Class::LeftBrack),
      '(' => self.push_c(Class::LeftParen),

      ']' => self.push_c(Class::RightBrace),
      '}' => self.push_c(Class::RightBrack),
      ')' => self.push_c(Class::RightParen),

      '.' => self.push_c(Class::Dot),
      ',' => self.push_c(Class::Comma),
      ':' => self.push_c(Class::Colon),
      ';' => self.push_c(Class::SemiColon),
      
      ' ' => self.advance(),
      '\n' => {
        self.advance();
        self.coords[0] += 1;
        self.coords[1] = 1;
      },
      '\0' => self.push_c(Class::Eof),
      '#' => while self.current() != '\n' { self.advance() }
      
      _ => {
        println!("{}", self.logger.error("invalid character", format!("{char:?} is not recognized."), coords));
        self.advance();
      },
    };
  }
}
