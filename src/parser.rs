use std::fmt::Display;

use crate::lexer::Lexer;
use crate::token::{Class, Token};
use crate::util::{Color, File, Has};
use crate::syntax::{Node, Expr};

pub struct Parser {
  filename: String,
  source: Vec<String>,
  pointer: usize,
  tokens: Vec<Token>,
}

impl Parser {
  pub fn init(lexer: &mut Lexer) -> Parser {
    let file = lexer.metadata();
    let source = file.lines().clone();
    let tokens = lexer.tokenize();

    let parser = Parser {
      filename: file.name(), 
      source, pointer: 0, tokens,
    };

    return parser;
  }
  pub fn parse(&mut self) -> Node {
    let mut body = vec![];
    while self.current().class != Class::Eof && self.pointer < self.tokens.len() -1 {
      body.push(self.parse_node());
    }

    Node::Compound { body }
  }
  pub fn metadata(&self) -> File {
    File::init(self.filename.clone(), self.source.join("\n"))
  }
  //============== Helper Functions ==============//
  fn advance(&mut self) {
    self.pointer += 1;
  }
  fn current(&self) -> Token {
    self.tokens[self.pointer].clone()
  }
  fn tokenth(&self, offset: isize) -> Token {
    let index = offset + self.pointer as isize;
    self.tokens[index as usize].clone()
  }
  fn consume<S:ToString>(&mut self, class: Class, info: S) -> Token  {
    let token = self.current(); self.advance();

    let info = if info.to_string().is_empty() 
      { format!("expected {} after {}", class, self.tokenth(-1).class) } else 
      { info.to_string() };

    if class != token.class {
      self.error(&token, info)
    }

    return token;
  }
  fn message<S:Display, V:Display>(&self, token: &Token, header: V, error: S) {
    let buffr = ' '.to_string().repeat(token.index[0].to_string().len());
		let space = ' '.to_string().repeat(token.index[1] -1);
		let value = '~'.to_string().repeat(token.text.len() - 1);
    let area = format!("{}[{}:{}]", self.filename, token.index[1], token.index[0]);

		let prob = vec![
			format!("{area} <- {header}: unexpected {}", token.class),
			format!("{} | {}", token.index[0], self.source[token.index[0] - 1]),
			format!("{} | {}^{} {}", buffr, space, value, error.to_string())
		].join("\n");

    println!("{prob}")
  }
  fn error<S:ToString>(&self, token: &Token, error: S) {
    self.message(token, "error".color(31), error.to_string());
    std::process::exit(1)
  }
  //=========== Expression Functions ============//
  fn fetch_expr(&mut self) -> Expr {
    let token = self.current();
    let mut expr = match token.class {
      Class::Identifier if self.tokenth(1).class == Class::LeftParen => self.build_fun_call(),

      Class::Identifier |
      Class::String | 
      Class::Number | 
      Class::Boolean => self.fetch_literal(),

      Class::LeftBrace => self.fetch_array(),
      Class::LeftBrack => self.build_lambda(),
      Class::LeftParen => self.fetch_wrapper(),

      Class::Null => Expr::NullVoid,

      _ => {
        self.error(&token, format!("expressions do not begin with {}", token.class));
        Expr::NullVoid
      }
    };

    while [Class::Colon, Class::BinaryOp, Class::BooleanOp, Class::LeftBrace].has(&self.current().class) {
      expr = match self.current().class {
        Class::Colon => self.build_attribute(Box::new(expr)),
        Class::BinaryOp => self.build_math_oper(Box::new(expr)),
        Class::BooleanOp=> self.build_bool_oper(Box::new(expr)),
        Class::LeftBrace=> self.fetch_arr_item(Box::new(expr)),
        Class::Keyword if self.current().text.as_str() == "as"
          => self.build_typecast(Box::new(expr)),
        _ => expr
      };
    }

    expr
  }
  fn fetch_literal(&mut self) -> Expr {
    let value = self.current(); self.advance();
    match value.class {
      Class::String  => Expr::String { value },
      Class::Number  => Expr::Number { value },
      Class::Boolean => Expr::Boolean { value },
      Class::Identifier => Expr::VarRef { value },
      Class::Null => Expr::NullVoid,

      _ => unreachable!()
    }
  }
  fn fetch_array(&mut self) -> Expr {
    let items = self.collect_series(
      [Class::LeftBrace, Class::RightBrace], Self::fetch_expr
    );

    Expr::Array { items }
  }
  fn fetch_arr_item(&mut self, parent: Box<Expr>) -> Expr {
    self.advance(); let index = Box::new(self.fetch_expr());
    self.consume(Class::RightBrace, "expected ']' after {expr} [ {expr}");

    Expr::ArrItem { parent, index }
  }
  fn fetch_typeref(&mut self) -> Expr {
    let mut arrays = 0;
    let base = self.consume(Class::Identifier, "expected base-type for type-refernce");
    
    while self.current().class == Class::LeftBrace {
      arrays += 1; self.advance();
      self.consume(Class::RightBrace, format!("expected ']' -- unmatched '['"));
    }
    Expr::TypeRef { base, arrays }
  }
  fn fetch_argument(&mut self) -> Expr {
    let name = self.consume(Class::Identifier, "expected argument name {identifier}");
    self.consume(Class::Colon, "expected ':' to divide arg name and type");
    let kind = Box::new(self.fetch_typeref());

    Expr::Argument { name, kind }
  }
  fn fetch_wrapper(&mut self) -> Expr {
    self.advance(); let expr = self.fetch_expr();
    self.consume(Class::RightParen, "unmatched '('");

    Expr::Wrapper { expr: Box::new(expr) }
  }

  fn build_bool_oper(&mut self, lhs: Box<Expr>) -> Expr {
    let oper = self.current(); self.advance();
    let rhs = Box::new(self.fetch_expr());

    Expr::BoolOper { lhs, rhs, oper }
  }
  fn build_math_oper(&mut self, lhs: Box<Expr>) -> Expr {
    let oper = self.current(); self.advance();
    let rhs = Box::new(self.fetch_expr());

    Expr::MathOper { lhs, rhs, oper }
  }
  fn build_typecast(&mut self, from: Box<Expr>) -> Expr {
    self.advance(); let to = Box::new(self.fetch_typeref());
    
    Expr::TypeCast { from, to }
  }
  fn build_lambda(&mut self) -> Expr {
    let args = self.collect_series(
      [Class::LeftBrack, Class::RightBrack], 
      Self::fetch_argument
    ); 

    let emit = if self.current().class == Class::Colon 
      { self.advance(); self.fetch_typeref() } else { Expr::NullVoid };

    self.consume(Class::Arrow, "expected '->' after almbda args");
    let body = Box::new(self.parse_node());

    Expr::Lambda { args, emit: Box::new(emit), body }
  }
  fn build_fun_call(&mut self) -> Expr {
    let name = self.current(); self.advance();
    let args = self.collect_series(
      [Class::LeftParen, Class::RightParen], Parser::fetch_expr
    );

    Expr::FunCall { name, args }
  }
  fn build_attribute(&mut self, parent: Box<Expr>) -> Expr {
    self.advance(); //: only called when ':' comes after expression
    if self.current().class != Class::Identifier {
      self.error(&self.current(), "referencing object fields must be in the form of function-call or {identifier");
    }

    let field = match self.tokenth(1).class {
      Class::LeftParen => self.build_fun_call(),
      _ => self.fetch_literal(),
    };

    Expr::Attribute { parent, field: Box::new(field) }
  }

  fn collect_series<T, F:Fn(&mut Parser) -> T>(&mut self, delims: [Class; 2], fetch: F) -> Vec<T> {
    let mut items = vec![];
    let mut able = true;

    self.consume(delims[0], format!("expected {} to start series", delims[0]));

    loop {
      let token = self.current();
      match token.class {
        Class::Comma => { self.advance(); able = true },
        Class::Eof => { 
          self.error(&token, format!("expected {} to terminate series.", delims[1])); break;
        },
        
        _ if token.class == delims[1] => { self.advance(); break; },
        _ if able => { able = false; items.push(fetch(self)) },

        _ => { self.error(&token, "expected ',' before next expression"); break; },
      }
    }

    return items; 
  }
  //=========== Statement Functions ============//
  fn parse_node(&mut self) -> Node {
    let mut term = true;
    let token = self.current();

    let node = match token.class {
      Class::Keyword => match token.text.as_str() {
        "fun" => { term = false; self.parse_func_define() }, 
        "while" => { term = false; self.parse_while_loop() },
        "if" => { term = false; self.parse_if_node() },
        "set" => self.parse_set_assign(),
        "var" => self.parse_var_assign(),
        "as" => { self.error(&token, "cannot begin statement with keyword `as`"); Node::NullVoid },
        "use" => self.parse_import(),
        "emit" => self.parse_emission(),
        _ => unreachable!()
      },
      Class::Identifier if self.tokenth(1).class == Class::Assign 
        => { self.parse_val_modify() },

      Class::SemiColon => { self.advance(); self.parse_node() }

      Class::LeftBrace => { term = false; self.parse_compound() },

      Class::Eof => Node::NullVoid,

      _ => Node::Expression { expr: self.fetch_expr() }
    };

    match &node {
      Node::SetAssign { value, .. } => if let Expr::Lambda { .. } = value { term = false },
      Node::VarAssign { value, .. } => if let Expr::Lambda { .. } = value { term = false },
      Node::ModifyVar { value, .. } => if let Expr::Lambda { .. } = value { term = false },
      _ => (),
    }


    if term {
      self.consume(Class::SemiColon, "expected ';' after statement");
    }

    node
  }
  fn parse_set_assign(&mut self) -> Node {
    self.advance();
    let name = self.consume(Class::Identifier, "expected identifier for constant name");
    
    let kind = if self.current().class == Class::Colon 
      { self.advance(); Some(self.fetch_typeref()) } else { None };

    self.consume(Class::Assign, "expected '=' for constant assignment");
    let value = self.fetch_expr();

    Node::SetAssign { name, kind, value }
  }
  fn parse_var_assign(&mut self) -> Node {
    self.advance();
    let name = self.consume(Class::Identifier, "expected identifier for constant name");
    
    let kind = if self.current().class == Class::Colon 
      { self.advance(); Some(self.fetch_typeref()) } else { None };

    self.consume(Class::Assign, "expected '=' for constant assignment");
    let value = self.fetch_expr();

    Node::VarAssign { name, kind, value }
  }
  fn parse_val_modify(&mut self) -> Node {
    let name = self.current(); self.advance();
    self.advance();
    let value = self.fetch_expr();

    Node::ModifyVar { name, value }
  }
  fn parse_func_define(&mut self) -> Node {
    self.advance();
    let name = self.consume(Class::Identifier, "expected function name after 'fun'");
    let args = self.collect_series([Class::LeftParen, Class::RightParen], Parser::fetch_argument);
    let kind = if self.current().class == Class::Colon
      { self.advance(); self.fetch_typeref() } else { Expr::NullVoid };

    self.consume(Class::Arrow, "expected '->' after fun def type");
    let body = Box::new(self.parse_node());

    Node::FuncDefinition { name, kind, args, body }
  }
  fn parse_compound(&mut self) -> Node {
    let mut body = vec![]; self.advance();

    loop {
      match self.current().class {
        Class::Eof => { self.error(&self.current(), "expected body delim ']', found <eof>") },
        Class::RightBrace => { self.advance(); break; },
        _ => body.push(self.parse_node())
      }
    };

    Node::Compound { body }
  }
  fn parse_if_node(&mut self) -> Node {
    self.advance(); 
    let bool = self.fetch_expr();
    let body = Box::new(self.parse_node());

    let other = if self.current().text == "else".to_string() {
      self.advance(); Box::new(self.parse_node())
    } else { Box::new(Node::NullVoid) };

    Node::IfStatement { bool, body, other }
  }
  fn parse_while_loop(&mut self) -> Node {
    self.advance(); let bool = self.fetch_expr();
    let body = Box::new(self.parse_compound());

    Node::WhileLoop { bool, body }
  }
  fn parse_import(&mut self) -> Node {
    self.advance();
    let mut path = vec![self.consume(Class::Identifier, "expected package name / exported member after 'use'")];

    while self.current().class == Class::Colon {
      self.advance();
      path.push(self.consume(Class::Identifier, "expected sub-package name / exported member after {pkg} ':' "));
    }

    Node::ImportPackage { path }
  }
  fn parse_emission(&mut self) -> Node {
    self.advance(); Node::ValueEmission { 
      expr: self.fetch_expr() 
    }
  }
}