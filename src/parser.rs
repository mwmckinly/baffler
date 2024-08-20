use crate::{lexer::Lexer, logger::Logger, syntax::{Expr, Node}, token::{Class, Token}, utils::Wrapper};

pub struct Parser {
  logger: Box<Logger>,
  tokens: Vec<Token>,
  pointer: usize,
}

impl Parser {
  pub fn init(lexer: Lexer) -> Parser {
    let ( tokens, logger ) = lexer.tokenize();

    return Parser {
      logger, tokens, pointer: 0,
    };
  }

  pub fn parse(mut self) -> (Vec<Node>, Box<Logger>) {
    let mut nodes = vec![];

    while self.current().class != Class::Eof {
      nodes.push(self.parse_node());
    }

    return (nodes, self.logger);
  }
}

impl Parser {
  fn error<V:ToString, S:ToString>(&self, header: V , message: S, token: &Token) {
    println!("{}", self.logger.error(header, message.to_string(), token));
    std::process::exit(1)
  }
}

impl Parser {
  fn advance(&mut self) {
    self.pointer += 1;
  }
  fn current(&self) -> Token {
    return self.tokens[self.pointer].clone();
  }
  fn tokenth(&self, offset: isize) -> Token {
    let index = (self.pointer as isize + offset) as usize;

    return if index < self.tokens.len() 
      { self.tokens[index].clone() } else 
      { self.tokens[self.tokens.len() - 1].clone() };
  }
  fn consume<S:ToString>(&mut self, class: Class, message: S) -> Token {
    let token = self.current(); self.advance();

    if token.class != class {
      self.error(format!("unexpected {}", token.class), message, &token);
    }

    return token;
  }
  fn grab(&mut self) -> Token {
    let token = self.current();
    self.advance();

    return token;
  }
  fn collect<T, F:Fn(&mut Parser) -> T>(&mut self, delims: [Class; 2], grab: F) -> Vec<T> {
    let mut items: Vec<T> = vec![];
    let mut able: bool = true;
    self.consume(delims[0], format!("expected {} to start series.", delims[1]));

    loop {
      let token = self.current();
      match token.class {
        Class::Comma => { self.advance(); able = true; },
        Class::Eof => {
          self.error("unterminated series", format!("expected {} to terminate series, but found <eof>", delims[1]), &token);
          break;
        },
        
        _ if token.class == delims[1] => { self.advance(); break; },

        _ => if able {
          able = false; items.push(grab(self))
        } else {
          self.error("invalid series", format!("expected ',' before next expression."), &token);
          break;
        }, 
      }
    }

    return items;
  }
}

impl Parser {
  pub fn expect_expr(&mut self) -> Expr {
    let token = self.current();

    let expr = match token.class {
      Class::Keyword => match token.text.as_str() {
        "if" => self.build_conditional(),
        _ => {
          self.error("invalid expression header", format!("expected expression header, but found keyword {}", token.text), &token);
          Expr::NullVoid
        }
      },

      Class::Identifier if self.tokenth(1).class == Class::LeftParen
        => self.fetch_fun_call(),

      Class::String
        | Class::Number
        | Class::Bool
        | Class::Identifier 
        => self.fetch_literal(),

      Class::LeftBrace => self.fetch_array(),
      Class::LeftParen => self.fetch_wrapper(),
      Class::LeftBrack => self.build_lambda(),

      _ => {
        self.error("invalid expression", format!("expected expression header, but found {}.", token.class), &token);
        Expr::NullVoid
      },
    };

    return match self.current().class {
      Class::LeftBrace => self.fetch_index(expr),
      Class::MathOp => self.build_operation(expr),
      Class::BoolOp => self.build_comparison(expr),
      Class::LogicOp => self.build_logic_chain(expr),

      _ => expr
    };
  }

  fn fetch_literal(&mut self) -> Expr {
    let value = self.grab();

    let expr = match value.class {
      Class::Identifier => Expr::VarRef { value },
      Class::String => Expr::String { value },
      Class::Number => Expr::Number { value },
      Class::Bool => Expr::Boolean { value },
      _ => unreachable!()
    };

    return expr;
  }
  fn fetch_array(&mut self) -> Expr {
    let value = self.collect(
      [Class::LeftBrace, Class::RightBrace],
      Self::expect_expr
    );

    return Expr::Array { value }
  }
  fn fetch_fun_call(&mut self) -> Expr {
    let name = self.grab();
    let args = self.collect(
      [Class::LeftParen, Class::RightParen], 
      Self::expect_expr
    );

    return Expr::FunCall { name, args }
  }
  fn fetch_wrapper(&mut self) -> Expr {
    self.advance(); let expr = self.expect_expr();
    self.consume(Class::RightParen, "expected ')' after '('");

    return expr;
  }
  fn fetch_index(&mut self, parent: Expr) -> Expr {
    self.advance(); let index = self.expect_expr().wrap();
    self.consume(Class::RightBrace, "expected ']' after '['");

    return Expr::Index { parent: parent.wrap(), index }
  }
  fn fetch_typeref(&mut self) -> Expr {
    let base = self.consume(Class::Identifier, "expected typeref name");
    let mut arrs = 0;
    while self.current().class == Class::LeftBrace {
      self.advance(); arrs += 1;
      self.consume(Class::RightBrace, "expected ']' after '['");
    }

    return Expr::TypeRef { base, arrs }
  }

  fn build_operation(&mut self, lhs: Expr) -> Expr {
    let oper = self.grab(); 
    let rhs = self.expect_expr();

    return Expr::MathOper { 
      lhs: lhs.wrap(), 
      oper, 
      rhs: rhs.wrap() 
    };
  }
  fn build_comparison(&mut self, lhs: Expr) -> Expr {
    let oper = self.grab(); 
    let rhs = self.expect_expr();

    return Expr::BoolOper { 
      lhs: lhs.wrap(), 
      oper, 
      rhs: rhs.wrap() 
    };
  }
  fn build_logic_chain(&mut self, lhs: Expr) -> Expr {
    let stich = self.grab(); 
    let rhs = self.expect_expr();

    return Expr::Chained { 
      lhs: lhs.wrap(), 
      stich, 
      rhs: rhs.wrap() 
    };
  }
  fn build_lambda(&mut self) -> Expr {
    let args = self.collect(
      [Class::LeftBrack, Class::RightBrack],
      Self::build_argument
    );

    let kind = if self.current().class == Class::Arrow {
      self.advance(); self.fetch_typeref()
    } else { Expr::NullVoid }.wrap();

    let body = self.parse_body().wrap();

    return Expr::Lambda { args, kind, body }
  }
  fn build_conditional(&mut self) -> Expr {
    self.advance(); 

    let cond = self.expect_expr().wrap();
    let body = self.parse_body().wrap();

    let other = if self.current().text.as_str() == "else" {
      self.advance(); self.parse_body()
    } else { Node::Expression { expr: Expr::NullVoid } }.wrap();

    Expr::IfExpr { cond, body, other }
  }
  fn build_argument(&mut self) -> Expr {
    let name = self.consume(Class::Identifier, "expected argument name");
    self.consume(Class::Colon, "expected ':' to divide param name and type.");
    let kind = self.fetch_typeref().wrap();

    return Expr::Argument { name, kind }
  }
}

impl Parser {
  fn parse_node(&mut self) -> Node {
    let token = self.current();
    let node = match token.class {
      Class::Keyword => match token.text.as_str() {
        "set" => self.parse_set_assign(),
        "var" => self.parse_var_assign(),
        "use" => self.parse_import_pkg(),
        "emit" => self.parse_emit_value(),
        "type" => self.parse_object_dec(),
        _ => unreachable!()
      },

      Class::Identifier if self.tokenth(1).class == Class::Assign =>
        self.parse_change_val(),

      _ => Node::Expression { expr: self.expect_expr() },
    };

    self.consume(Class::SemiColon, "expected ';' to terminate statement");

    return node;
  }
  
  fn parse_object_dec(&mut self) -> Node {
    self.advance(); let name = self.grab();
    let attrs = self.collect([Class::LeftBrack, Class::RightBrack], |s| {
      let name = s.consume(Class::Identifier, "expected attribute name");
      s.consume(Class::Colon, "expected ':' to divide attr name and type");
      let kind = s.fetch_typeref();

      (name, kind)
    }).into_iter().collect();

    return Node::ObjectDec { name, attrs };
  }
  fn parse_set_assign(&mut self) -> Node {
    self.advance();
    let name = self.consume(Class::Identifier, "expected variable name");
    self.consume(Class::Assign, "expected '=' after `set {name}`");
    let value = self.expect_expr();

    return Node::SetAssign { name, value }
  }
  fn parse_var_assign(&mut self) -> Node {
    self.advance();
    let name = self.consume(Class::Identifier, "expected variable name");
    self.consume(Class::Assign, "expected '=' after `set {name}`");
    let value = self.expect_expr();

    return Node::VarAssign { name, value }
  }
  fn parse_change_val(&mut self) -> Node {
    let name = self.consume(Class::Identifier, "expected variable name");
    self.consume(Class::Assign, "expected '=' after `set {name}`");
    let value = self.expect_expr();

    return Node::ChangeVal { name, value }
  }
  fn parse_import_pkg(&mut self) -> Node {
    self.advance(); let mut path = vec![
      self.consume(Class::Identifier, "expected package name.")
    ];

    while self.current().class == Class::Colon {
      self.advance(); path.push(
        self.consume(Class::Identifier, "expected package name.")
      );
    }

    return Node::ImportLib { path };
  }
  fn parse_emit_value(&mut self) -> Node {
    self.advance(); return Node::EmitValue { 
      value: self.expect_expr() 
    };
  }

  fn parse_body(&mut self) -> Node {
    let mut body = vec![];
    self.consume(Class::LeftBrack, "expected '{' to begin body node.");

    loop {
      match self.current().class {
        Class::Eof => { self.error("unterminated block", "expected body delim '}', found <eof>", &self.current()) },
        Class::RightBrack => { self.advance(); break; },
        _ => body.push(self.parse_node())
      }
    };

    return Node::Compound { value: body };
  }
}