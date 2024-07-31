use std::fs::read_to_string;

use crate::token::{Class, Token};
use crate::util::{Color, Has};

pub struct Lexer {
  filename: String,
  source: Vec<char>,
  tokens: Vec<Token>,
  
  pointer: usize,
  position: [usize; 2],
}

impl Lexer {
  pub fn init(filename: String) -> Lexer {
    let source = if let Ok(mut code) = read_to_string(&filename) { 
      code.push('\0'); code 
    } else { 
      println!(
        "{}: invalid filename -- {} does not exist.", 
        "error".color(31), filename
      ); std::process::exit(1) 
    }.chars().collect();
    
    let lexer = Lexer {
      filename, source,
      tokens: vec![],
      pointer: 0,
      position: [1, 1],
    };

    return lexer;
  }
  pub fn metadata(&self) -> (String, Vec<String>) {
    let lines: String = self.source.clone().iter().collect();
    let lines = lines.split("\n").map(|x| x.to_string());

    (self.filename.clone(), lines.collect())
  }
  //========== Helper Functions ==========//
  fn advance(&mut self) {
    self.position[1] += 1;
    self.pointer += 1;
  } 
  fn current(&self) -> char {
    self.source[self.pointer]
  }
  fn append(&mut self, class: Class) {
    let token = Token::new(class, self.current(), self.position);
    self.advance(); self.tokens.push(token);
  }
  //============= Tokenizing ============//
  fn next_token(&mut self) {
    let current = self.current();
    let index = self.position;
    match current {
      _ if self.current().is_alphabetic() => {
        let mut text: String = "".into();

        let check = |c: char| c.is_alphanumeric() || c == '_'; 

        while check(self.current()) {
          text.push(self.current()); 
          self.advance();
        }

        let class = match text.as_str() {
          "fun" | "set" | "var" | "as" |
          "use" | "emit" | "if" | "else" => Class::Keyword,
          "null" => Class::Null,
          "true" | "false" => Class::Boolean,
          _ => Class::Identifier,
        };

        self.tokens.push(Token::new(class, text, index));
      },
      _ if self.current().is_numeric() => {
        let mut text: String = "".into(); let mut dots = 0;
        let check = |c: char| c.is_numeric() || c == '.';
        
        while check(self.current()) {
          if self.current() == '.' { dots += 1; }
          text.push(self.current()); 
          self.advance(); if dots == 2 { break; }
        }

        self.tokens.push(Token::new(Class::Number, text, index));
      },
      _ if self.current() == '"' => {
        let mut text = "".to_string();
        self.advance();

        while self.current() != '"' {
          if self.pointer == self.source.len() {
            println!("{}: found <eof> before string delim '\"'", "error".color(31));
            break;
          }
          if self.current() == '\\' 
            { self.advance(); text += "\\"; }

          text.push(self.current());
          self.advance();
        } self.advance();

        self.tokens.push(Token::new(Class::String, text, index));
      },

      '[' => self.append(Class::LeftBrace), 
      '{' => self.append(Class::LeftBrack), 
      '(' => self.append(Class::LeftParen), 
      ']' => self.append(Class::RightBrace),
      '}' => self.append(Class::RightBrack),
      ')' => self.append(Class::RightParen),
    
      '.' => self.append(Class::Dot),   
      ',' => self.append(Class::Comma), 
      ':' => self.append(Class::Colon), 
      ';' => self.append(Class::SemiColon),
    

      '|' | '&' | '!' => self.append(Class::LogicOp),

      '+' | '*' | '/' | '%' => {
        let mut text = self.current().to_string(); self.advance();
        if self.current() == '=' { text += "="; self.advance(); }

        self.tokens.push(Token::new(Class::BinaryOp, text, index));
      },

      '-' => {
        self.advance();
        let (text, class) = if self.current() == '=' {
          self.advance(); ("-=", Class::BinaryOp)
        } else if self.current() == '>' {
          self.advance(); ("->", Class::Arrow)
        } else {
          ("-", Class::BinaryOp)
        };
        self.tokens.push(Token::new(class, text, index));
      },

      '<' | '>' => {
        let mut text = self.current().to_string();
        self.advance(); if self.current() == '=' 
          { text += "="; self.advance(); }

        self.tokens.push(Token::new(Class::BooleanOp, text, index));
      },

      '=' => {
        self.advance(); 
        if self.current() == '=' {
          self.advance();
          self.tokens.push(Token::new(Class::BooleanOp, "==", index))
        } else {
          self.tokens.push(Token::new(Class::Assign, "=", index))
        }
      }

      '#' => while !['\n', '\0'].has(&self.current()) { self.advance(); }

      ' ' => self.advance(),
      '\n' => {
        self.advance();
        self.position[1] = 1;
        self.position[0] += 1;
      },
      '\0'=> { self.position[1] += 1; self.append(Class::Eof) },

      _ => {
        println!(
          "{}[{}:{}] --> {}: invalid character {:?}", 
          self.filename, index[0], index[1], "error".color(31), current
        );
        self.advance();
      }
    }
  }
  pub fn tokenize(&mut self) -> Vec<Token> {
    while self.pointer < self.source.len() 
      { self.next_token(); }

    return self.tokens.clone();
  }
}