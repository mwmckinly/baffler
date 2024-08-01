use std::{borrow::Borrow, collections::HashMap, fmt::Display};

use crate::{parser::Parser, syntax::{Expr, Node}, token::Token, util::{Color, SourceInfo}};

#[derive(Clone, PartialEq, Debug)]
pub enum IRType {
  String, 
  Number, 
  Boolean,
  Array { of: Box<IRType> },
  Struct { fields: HashMap<String, IRType> },
  None,
  Null,
}

impl Display for IRType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let value: String = match self {
      IRType::String => "ir:string".into(),
      IRType::Number => "ir:number".into(),
      IRType::Boolean => "ir:boolean".into(),
      IRType::Array { of } => format!("ir:array<{of}>"),
      IRType::Struct { .. } => "ir:struct".into(),
      IRType::None => "ir:none".into(),
      IRType::Null => "ir:null".into(),
    };

    write!(f, "{value}")
  }
}

#[derive(Clone, PartialEq, Debug)]
pub enum IRValue {
  String { value: Vec<char> },
  Number { value: f64 },
  Boolean { value: bool },
  Array { value: Vec<IRValue> },
  Struct { fields: HashMap<String, IRValue> },
  Null
}

impl IRValue {
  fn ir_type(&self) -> IRType {
    match self {
      IRValue::String { .. } => IRType::String,
      IRValue::Number { .. } => IRType::Number,
      IRValue::Boolean { .. } => IRType::Boolean,
      IRValue::Array { value } => IRType::Array { of: Box::new(value[0].ir_type()) },
      IRValue::Struct { fields } => {
        let mut parts = HashMap::new();

        for (name, field) in fields {
          parts.insert(name.clone(), field.ir_type());
        }

        IRType::Struct { fields: parts }
      },
      IRValue::Null => todo!(),
    }
  }
}

#[derive(Clone, PartialEq, Debug)]
pub enum IRNode {
  AssignVariable { name: String, value: IRValue, mutable: bool },
  CallFunction { name: String, args: Vec<IRValue> },
  DeclareFunction { name: String, args: HashMap<String, IRType>, kind: IRType, body: Box<IRNode> },

  EvaluateBinaryExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 0: +, 1: -, 2: *, 3: /, 4: %
  EvaluateBooleanExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 10: ==, 11: <, 12: >, 13: <=, 14: >=, 15: !=

  ReturnValue { value: IRValue },
  Compound { body: Vec<IRNode> },
}

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
  Function { kind: IRType },
  Variable { kind: IRType, mutable: bool }, 
  None
}

#[derive(Debug, Clone)]
pub struct Scope {
  symbols: HashMap<String, Symbol>,
  parent: Option<usize>
}

impl Scope {
  pub fn new(parent: Option<usize>) -> Self {
    return Scope { symbols: HashMap::new(), parent, };
  }
}

pub struct Analyzer {
  program: Vec<Node>,
  pointer: usize,
  
  scopes: Vec<Scope>,
  current: usize,

  filename: String,
  source: Vec<String>,
}

impl Analyzer {
  pub fn init(parser: &mut Parser) -> Self {
    let program = if let Node::Compound { body } = parser.parse() { body } else { unreachable!() };

    let file = parser.metadata();

    Analyzer {
      program, pointer: 0,
      scopes: vec![Scope::new(None)],
      current: 0, filename: file.name(),
      source: file.lines(),
    }
  }

  fn scope(&self) -> &Scope {
    self.scopes.get(self.current).unwrap()
  }
  fn enter(&mut self) {
    self.scopes.push(Scope::new(Some(self.current)));
    self.current = self.scopes.len() - 1;
  }
  fn leave(&mut self) {
    let scope = if let Some(parent) = self.scope().parent 
      { parent } else { println!("already in root scope"); 0 };
    self.current = scope;
  }

  pub fn lookup<S:ToString>(&self, var: S) -> Option<&Symbol> {
    return self.scope().symbols.get(&var.to_string()).clone();
  }

  fn message<V:Display, S:ToString, Value:SourceInfo>(&self, node: &Value, kind: String, header: V, message: S) {
    let (line, col, len) = node.info();

    let buffr = ' '.to_string().repeat(line.to_string().len());
    let space = ' '.to_string().repeat(col -1);
    let value = '~'.to_string().repeat(len - 1);

    let info = vec![
      format!("{}[{}:{}] --> {kind}: {header}", self.filename, line, col),
      format!("{} | {}", line, self.source[line - 1].clone()),
      format!("{} | {}^{} {}", buffr, space, value, message.to_string())
    ].join("\n");

    println!("{info}")
  }
  fn error<V:Display, S:ToString, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "error".color(31), header, message)
  }
  fn warn<V:Display, S:ToString, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "warning".color(33), header, message)
  }

  fn get_ir_type(&self, expr: &Expr) -> IRType {
    match expr {
      Expr::String { .. } => IRType::String,
      Expr::Number { .. } => IRType::Number,
      Expr::Boolean { .. } => IRType::Boolean,
      Expr::VarRef { value } => {
        let var = self.lookup(&value.text);

        if var.is_none() {
          self.error(expr, "symbol not found", format!("{} is either not in scope or not defined", value.text));
        };

        if let Symbol::Variable { kind, .. } = var.unwrap() {
          return kind.clone()
        } else {
          self.error(expr, "cannot reference function", format!("must call function when referencing, cannot just use function name"));
          IRType::Null
        }
      },
      Expr::Lambda { emit, .. } => self.get_ir_type(emit),
      Expr::Array { items } => {
        let of = if items.len() == 0 
          { IRType::Null } else { self.get_ir_type(&items[0]) };

        IRType::Array { of: Box::new(of) }
      },
      Expr::ArrItem { parent, .. } => {
        if let IRType::Array { of } = self.get_ir_type(&parent) 
          { *of } else { unreachable!() }
      },
      Expr::MathOper { lhs, rhs, oper } => {
        let lhs = self.get_ir_type(lhs);
        let rhs = self.get_ir_type(rhs);

        if lhs != rhs {
          self.error(expr, "invalid types", format!("cannot performe a binary operation on {} with {}", lhs, rhs));
        }

        if lhs == IRType::String && oper.text != "+" {
          self.error(expr, "invalid operation", format!("'{}' is not a valid operator for the type string.", oper.text));
        }

        return lhs;
      },
      Expr::BoolOper { lhs, rhs, .. } => {
        let lhs = self.get_ir_type(lhs);
        let rhs = self.get_ir_type(rhs);

        if lhs != rhs {
          self.warn(expr, "invalid types", format!("attempting to compare different types, this will always be false"))
        }

        return IRType::Boolean;
      },
      Expr::FunCall { name, .. } => {
        let func = self.lookup(&name.text);

        let func = if func.is_some() {
          func.unwrap()
        } else {
          self.error(expr, "symbol not found", format!("{} is either not in scope or not defined.", name.text));
          &Symbol::None
        };

        if let Symbol::Function { kind, .. } = func 
          { kind.clone() } else { unreachable!() }
      },
      Expr::Wrapper { expr } => self.get_ir_type(expr),
      Expr::Argument { kind, .. } => self.get_ir_type(kind),
      Expr::TypeRef { base, arrays } => {
        let mut base = match base.text.as_str() {
          "str" => IRType::String,
          "num" => IRType::Number,
          "bool" => IRType::Boolean,
          "null" => IRType::Null,
          _ => {
            self.error(expr, "unknown type", format!("the type {} does not exist or is not in scope.", base.text));
            IRType::Null
          }
        };
        for _ in 0..*arrays { base = IRType::Array { of: Box::new(base) }; }

        return base;
      },
      Expr::TypeCast { to, .. } => self.get_ir_type(to),
      Expr::NullVoid => IRType::Null,
      Expr::Attribute { parent, .. } => {
        let class = if let Some(class) = self.lookup(parent) {
          class
        } else { 
          self.error(expr, "symbol not found", format!("could not find {} in scope", parent));
          &Symbol::None 
        };

        todo!()
      },
    }
  }

  fn add_symbol<S:ToString>(&mut self, name: S, symbol: Symbol) {
    let scope = self.scopes.get_mut(self.current).unwrap();
    scope.symbols.insert(name.to_string(), symbol);
  }
  fn add_variable(&mut self, node: &Node) {
    let mut add_var = |name, kind: &Option<Expr>, value, mutable| {
      if let Some(_) = self.lookup(&name) {
        self.error(node, "symbol already in scope", format!("{} has already been declared.", &name));
      }
  
      let kind = if kind.is_some() {
        self.get_ir_type(&kind.clone().unwrap())
      } else { IRType::None  };
  
      let value = self.get_ir_type(&value);
  
      if value != kind && kind != IRType::None {
        self.error(node, "invalid type", format!("{} has been declared to be a {}, but assigned to {}. This is not valid.", &name, kind, value))
      }
  
      self.add_symbol(name, Symbol::Variable { kind, mutable });
    };

    match node {
      Node::SetAssign { name, kind, value } => add_var(name, kind, value.clone(), false),
      Node::VarAssign { name, kind, value } => add_var(name, kind, value.clone(), true),
      Node::ObjectField { field, kind } => todo!(),
      Node::ObjExtention { name, body } => todo!(),
      _ => todo!()
    };
  }
  fn check_modification(&mut self, node: &Node) {}
  fn add_function(&mut self, node: &Node) {
    let (name, kind, nodes) = if let Node::FuncDefinition { name, kind, args: _, body } = node {
      (name, kind, body )
    } else { unreachable!() };

    let kind = self.get_ir_type(kind);
    let body = if let Node::Compound { body } = *nodes.clone() 
      { body } else { vec![*nodes.clone()] };

    self.enter();
    for statement in body {
      self.analyze_node(&statement);
      if let Node::ValueEmission { expr } = statement.clone() {
        if self.get_ir_type(&expr) != kind {
          self.error(&statement, "invalid types", format!("{} was declared to have a return type of {}, but its returning a {} here.", &name, kind, self.get_ir_type(&expr)))
        }
      }
    }
    self.leave();

    self.add_symbol(name, Symbol::Function { kind })
  }

  fn analyze_node(&mut self, node: &Node) {
    match node {
      Node::SetAssign { .. } |
      Node::DeclareObject { .. } |
      Node::VarAssign { .. } => self.add_variable(node),
      Node::ModifyVar { .. } => self.check_modification(node),
      Node::FuncDefinition { .. } => self.add_function(node),
      Node::ObjExtention { name, body } => todo!(),
      Node::Expression { expr } => self.analyze_expr(&expr),
      _ => todo!()
    }
  }

  fn analyze_expr(&mut self, expr: &Expr) {

  }

  pub fn analyze(&mut self) {
    for node in self.program.clone() {
      self.analyze_node(&node);
    }
  }
}