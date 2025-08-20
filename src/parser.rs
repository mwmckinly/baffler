use crate::blocks::{Node, Expr, Argument};
use crate::lexer::{Lexer};
use crate::token::{Token, Class};
use crate::logger::{Logger, LogLevel};

pub struct Parser {
  pub(crate) logger: Logger,
  tokens: Vec<Token>,
  pointer: usize,
}

impl Parser {
  pub fn init(lexer: Lexer) -> Parser {
    let (tokens, logger) = lexer.tokenize();

    let parser =  Parser {
      logger, tokens, pointer: 0
    };

    return parser;
  }

  fn view(&self, offset: isize) -> Option<&Token> {
    let index = self.pointer as isize + offset;
    self.tokens.get(index as usize)
  }
  fn curr(&self) -> &Token {
    return self.view(0).unwrap_or_else(|| self.tokens.last().unwrap());
  }
  fn walk(&mut self) {
    self.pointer += 1;
  }
  fn pull(&mut self) -> Token {
    let token = self.curr().clone();
    self.walk();
    return token;
  }
  fn take(&mut self, class: Class) -> Result<Token, Token> {
    let token = self.curr().clone();
    self.walk();

    let res = match token.class == class {
      true => Ok(token), false => Err(token)
    };

    return res;
  }

  fn collect<T, F:FnMut(&mut Parser) -> T>(&mut self, term: Class, sep: Class, mut parser: F) -> Vec<T> {
    let mut items = vec![];
    let mut fetchable = true;

    loop {
      let cur = self.curr().clone();

      match cur.class {
        c if c == sep => { self.walk(); fetchable = true },
        c if c == term => { self.walk(); break; },

        _ if fetchable => {
          items.push(parser(self));
          fetchable = false;
        },

        _ => {
          self.logger.log(LogLevel::Error, &cur, "Must seperate array items with a comma");
          break;
        }
      }
    };

    return items;
  }

  pub fn test(&mut self) -> () {
    let expr = self.parse_expr();
    println!("{expr}")
  }
}

impl Parser {
  fn parse_expr(&mut self) -> Expr {
    let token = self.curr().clone();

    let value: Expr = match token.class {
        Class::Identifier => match self.view(1).unwrap().class {
          Class::LParen => self.parse_fun_call(),
          _ => self.parse_var_ref(),
        },
        Class::String => self.parse_string(),
        Class::Number => self.parse_number(),
        Class::Boolean => self.parse_boolean(),

        Class::LBrace => self.parse_array(),
        Class::LBrack => self.parse_inline(),
        Class::LParen => self.parse_container(),
        _ => {
          let msg = format!("expressions cannot start with {}.", token.class);
          self.logger.log(LogLevel::Error, &token, msg);
          
          panic!()
        },
    };

    let expr: Expr = match self.curr().class {
      Class::Operator |
      Class::Comparator => self.parse_binary(value),
      _ => value,
    };

    return expr;
  }
  
  fn parse_string(&mut self) -> Expr {
    let value = self.pull();
    return Expr::String { value };
  }
  fn parse_number(&mut self) -> Expr {
    let value = self.pull();
    return Expr::Number { value };
  }
  fn parse_boolean(&mut self) -> Expr {
    let value = self.pull();
    return Expr::Boolean { value };
  }
  fn parse_var_ref(&mut self) -> Expr {
    let value = self.pull();
    return Expr::Variable { value };
  }

  fn parse_fun_call(&mut self) -> Expr {
    let name = self.pull(); self.walk();
    let args = self.collect(Class::RParen, Class::Comma, Self::parse_expr);

    return Expr::FunCall { name, args }
  }
  fn parse_array(&mut self) -> Expr {
    self.walk();

    let value = self.collect(Class::RBrace, Class::Comma, Self::parse_expr);

    return Expr::Array { value }
  }
  fn parse_inline(&mut self) -> Expr {
    self.walk();
    
    let args = self.collect(Class::RBrack, Class::Comma, Self::parse_var_ref);
    let code = Box::new(self.build_node());

    return Expr::Inline { args, code }
  }
  fn parse_container(&mut self) -> Expr {
    let start = self.pull();
    let value = self.parse_expr();

    if self.take(Class::RParen).is_err() {
      self.logger.log(LogLevel::Error, &start, "Cannot leave opening '(' unclosed.");
      self.logger.log(LogLevel::Warning, &value, "Has no termination character.");
    }

    return value;
  }
  fn parse_binary(&mut self, first: Expr) -> Expr {
    let lhs = Box::new(first);
    let op = self.pull();
    let rhs = self.parse_expr();

    return Expr::BinaryOp { lhs, op, rhs: Box::new(rhs) }
  }

  fn parse_type(&mut self) -> Expr {
    let name = self.take(Class::Identifier).unwrap_or_else(|t| {
      self.logger.log(LogLevel::Error, &t, "expected type name");
      panic!()
    });

    let mut arrays = 0;

    while self.curr().class == Class::LBrace {
      self.walk();
      self.take(Class::RBrace).unwrap_or_else(|t| {
        self.logger.log(LogLevel::Error, &t, "Unclosed '[' character"); t
      });

      arrays += 1;
    }

    return Expr::Type { name, arrays }
  }
  fn parse_argument(&mut self) -> Argument {
    todo!()
  }
}

impl Parser {
  fn build_node(&mut self) -> Node {
    let token = self.curr().clone();

    let node = match token.class {
      Class::Keyword => match token.text.as_str() {
        "fun" => self.build_fun_def(),
        "set" => self.build_set_val(),
        "var" => self.build_mut_val(),
        "import" => self.build_import(),
        "emit" => self.build_emit_val(),
        "if" => self.build_condition(),
        _ => unreachable!()
      },
      _ => Node::Expr(self.parse_expr()),
    };

    return node;
  }

  fn build_fun_def(&mut self) -> Node {
    self.walk();
    let name = self.take(Class::Identifier).unwrap_or_else(|t| {
      self.logger.log(LogLevel::Error, &t, "must have identifier after `fun` keyword");
      return t;
    });
    let args = self.collect(Class::RParen, Class::Comma, Parser::parse_argument);
    let emit = if self.curr().class == Class::Arrow 
      { self.walk(); self.parse_type() } else 
      { Expr::Unknown(Box::new(Expr::Variable { value: name.clone() })) };

    let node = Box::new(self.build_node());

    return Node::FunDefine { name, args, emit, node };
  }
  fn build_set_val(&mut self) -> Node { todo!() }
  fn build_mut_val(&mut self) -> Node { todo!() }
  fn build_import(&mut self) -> Node { todo!() }
  fn build_emit_val(&mut self) -> Node { todo!() }
  fn build_condition(&mut self) -> Node { todo!() }

  pub fn build_tree(mut self) -> (Logger, Vec<Node>) {
    let mut nodes = vec![];

    while self.curr().class != Class::Eof {
      nodes.push(self.build_node());
    }

    return (self.logger, nodes);
  }
}