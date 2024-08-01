use std::{collections::HashMap, fmt::Display};
use serde::Serialize;

use crate::parser::Parser;
use crate::syntax::{Expr, Node};
use crate::util::{Color, SourceInfo, Has};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Type { 
  Number, String, Boolean, 
  Array { of: Box<Type> }, 
  Struct { fields: HashMap<String, Type> },
  NullVoid, None 
}
impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().strip_suffix("\n").unwrap())
  }
}

#[derive(Debug, Clone)]
enum Symbol {
  Function { kind: Type, args: Vec<Type> },
  Variable { kind: Type, mutable: bool },
}

#[derive(Debug, Clone)]
struct Scope {
  parent: Option<Box<Scope>>,
  symbols: HashMap<String, Symbol>,
}

impl Scope {
  pub fn new(parent: Scope) -> Scope {
    Scope { parent: Some(Box::new(parent)), symbols: HashMap::new() }
  }
  pub fn add<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.symbols.insert(name.to_string(), symbol);
  }
  pub fn get<S:ToString>(&self, name: S) -> Option<&Symbol> {
    self.symbols.get(&name.to_string())
  }
}

pub struct TypeChecker {
  program: Vec<Node>,
  scope: Scope,

  types: HashMap<String, Type>,

  filename: String,
  source: Vec<String>,
}

impl TypeChecker {
  pub fn init(parser: &mut Parser) -> Self {
    let file = parser.metadata();
    let program = parser.parse();

    let program = if let Node::Compound { body } = program 
      { body } else { vec![program] };

    let primatives: HashMap<String, Type> = vec![
      ("str", Type::String),
      ("num", Type::Number),
      ("bool", Type::Boolean),
      ("null", Type::NullVoid),
    ].iter().map(|(l, r)| {
      (l.to_string(), r.clone())
    }).into_iter().collect();

    TypeChecker {
      program,
      scope: Scope { parent: None, symbols: HashMap::new() },
      types: primatives,
      filename: file.name(),
      source: file.lines(),
    }
  }
  pub fn check(&mut self) {
    self.program.clone().iter().for_each(|node| self.check_node(node));
  }
}

impl TypeChecker {
  pub fn message<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, kind: String, header: V, message: S) {
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
  fn error<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "error".color(31), header, message)
  }
  fn warn<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "warning".color(33), header, message)
  }

  fn lookup<S:ToString>(&self, name: S) -> Option<&Symbol> {
    return self.scope.get(name);
  }
  fn insert<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.scope.add(name.to_string().clone(), symbol);
  }
  fn enter(&mut self) {
    self.scope = Scope::new(self.scope.clone());
  }
  fn leave(&mut self) {
    if let Some(scope) = self.scope.parent.take() 
      { self.scope = *scope; } else 
      { println!("{}: attempting to leave root scope", "error".color(31)) };
  }

  fn get_type(&self, expr: &Expr) -> Type {
    match expr {
      Expr::String { .. } => Type::String,
      Expr::Number { .. } => Type::Number,
      Expr::Boolean { .. } => Type::Boolean,
      Expr::VarRef { value } => {
        let var = self.lookup(&value.text);

        if var.is_none() {
          self.error(expr, "symbol not found", format!("{} is either not in scope or not defined", value.text));
          return Type::None;
        };

        if let Symbol::Variable { kind, .. } = var.unwrap() {
          return kind.clone()
        } else {
          self.error(expr, "cannot reference function", format!("must call function when referencing, cannot just use function name"));
          Type::None
        }
      },
      Expr::Lambda { emit, .. } => self.get_type(emit),
      Expr::Array { items } => {
        if items.len() == 0 {
          return Type::Array { of: Box::new(Type::None) }
        }

        Type::Array { of: Box::new(self.get_type(&items[0])) }
      },
      Expr::ArrItem { parent, .. } => {
        let base = self.get_type(&parent);
        let base = if let Type::Array { of } = base {
          *of
        } else if let Type::String = base {
          Type::String
        } else {
          self.error(expr, "invalid operation", format!("a {} cannot be indexed", base));
          Type::None
        };

        return base;
      },
      Expr::MathOper { lhs, rhs, oper } => {
        let lhs = self.get_type(lhs);
        let rhs = self.get_type(rhs);
        let oper = oper.text.as_str();

        if lhs == Type::String {
          if oper != "+" {
            self.error(expr, "invalid operation", format!("The only binary operation avalible for strings is concatination '+'."))
          }
          return Type::String;
        }

        if lhs != rhs {
          self.error(expr, "invalid operation", format!("cannot perform a binar yoperation on {} with {}", lhs, rhs));
        };

        Type::Number
      },
      Expr::BoolOper { lhs, rhs, .. } => {
        let lhs = self.get_type(lhs);
        let rhs = self.get_type(rhs);

        if lhs != rhs {
          self.warn(expr, "incompatable types", format!("this condition will always be false because {lhs} and {rhs} are not the same type"));
        }

        Type::Boolean
      },
      Expr::FunCall { name, args } => {
        let found = self.lookup(name.text.clone());
        
        if found.is_none() {
          self.error(expr, "symbol not in scope", format!("{name} is not defined in the current scope."));
          return Type::None;
        }; 
        
        if let Some(Symbol::Function { kind, args: params }) = found {
          if params.len() != args.len() {
            self.error(expr, "invalid arguments", format!(
              "{name} was declared to have {} args, but here its called with {}", 
              params.len(), args.len()
            ));
          }

          for i in 0..args.len() {
            let arg = self.get_type(&args[i]);
            if arg != params[i] {
              self.error(expr, "invalid types", format!("{name}'s arg at index {i} should be of type {}, not {arg}", params[i]))
            }
          }

          return kind.clone();
        } else { return Type::None };
      },
      Expr::Object { kind, args } => {
        let object = if let Some(kind) = self.types.get(&kind.text) {
          kind.clone()
        } else {
          self.error(expr, "symbol not in scope", format!("{kind} is not a defined type in the current scope."));
          return Type::None
        };

        return object;
      },
      Expr::Wrapper { expr } => self.get_type(expr),
      Expr::Argument { kind, .. } => self.get_type(kind),
      Expr::TypeRef { base, arrays } => {
        let mut kind = if let Some(kind) = self.types.get(&base.text) {
          kind.clone()
        } else { 
          self.error(expr, "type does not exist", format!("{} is not a defined type within the current scope.", base.text));
          Type::None 
        };

        for _ in 0..*arrays {
          kind = Type::Array { of: Box::new(kind) }
        }

        return kind;
      },
      Expr::TypeCast { from, to } => todo!("type casting is not yet supported - {} -> {}", self.get_type(from), self.get_type(to)),
      Expr::Attribute { parent, field } => {
        let attr = if let Expr::VarRef { value } = *field.clone() {
          value.text
        } else if let Expr::FunCall { name, args } = *field.clone() {
          let params: Vec<String> = args.iter().map(|param| {
            self.get_type(param).to_string()
          }).collect();

          format!("{}({})", name.text, params.join(", "))
        } else { unreachable!() };

        match *parent.clone() {
          Expr::String { .. } => println!("string attributes are not yet supported - {field}"),
          Expr::Number { .. } => println!("numeric attributes are not yet supported - {field}"),
          Expr::Boolean { .. } => println!("boolean attributes are not yet supported - {field}"),
          Expr::VarRef { .. } => todo!(),
          Expr::Array { .. } => todo!(),
          _ => {
            self.error(expr, "attribute does not exist", format!("The attribute {} does not exist on type {}", attr, self.get_type(parent)))
          },
        };

        Type::None
      },
      Expr::NullVoid => Type::NullVoid,
    }
  }

  fn check_node(&mut self, node: &Node) {
    match node {
      Node::SetAssign { .. } |
      Node::VarAssign { .. } => self.check_assignment(node),
      Node::ModifyVar { .. } => self.check_reassign(node),
      Node::FuncDefinition { .. } => self.check_function(node),
      Node::DeclareObject { .. } => self.add_type(node),
      Node::Compound { .. } => self.check_compound(node),
      Node::Expression { expr } => self.check_expression(expr),
      _ => (),
    }
  }
  fn check_assignment(&mut self, node: &Node) {
    let (name, kind, value, mutable) = if let Node::SetAssign { name, kind, value } = node {
      (name.text.clone(), kind, value, false)
    } else if let Node::VarAssign { name, kind, value } = node {
      (name.text.clone(), kind, value, true)
    } else { unreachable!() };

    if self.lookup(&name).is_some() {
      self.error(node, "symbol already exists", format!("{name} has already been assigned in the current scope."));
    };

    let val_t = self.get_type(&value);
    let kind = if kind.is_some() 
      { self.get_type(&kind.clone().unwrap()) } else 
      { val_t.clone() };


    if kind != val_t && val_t != Type::NullVoid {
      self.error(node, "mismatched types", format!("{name} was declared to be a {kind}, but is assigned to be a {val_t}."))
    }

    self.insert(name, Symbol::Variable { kind, mutable })
  }
  fn check_reassign(&mut self, node: &Node) {
    let (name, kind) = if let Node::ModifyVar { name, value } = node {
      (name.text.clone(), self.get_type(value))
    } else { unreachable!() };

    let symbol = self.lookup(&name);

    let symbol = if symbol.is_none() {
      self.error(node, "symbol does not exist", format!("{name} has not been declared yet."));
      return;
    } else { symbol.unwrap() };

    let symbol = if let Symbol::Variable { kind, mutable } = symbol {
      println!("{mutable}");
      (kind, mutable)
    } else {
      self.error(node, "invalid operation", format!("{name} is not a mutable variable."));
      return;
    };

    if symbol.0 != &kind && symbol.0 != &Type::NullVoid {
      self.error(node, "mismatched types", format!("{name} was declared to be a {}, but is attempting to be a {}.", symbol.0, kind));
      return;
    }

    if !symbol.1 {
      self.error(node, "invalid operation", format!("{name} was declared to be a constant, you cannot modify its value"));
    } 
  }
  fn check_function(&mut self, node: &Node) {
    let (name, kind, args, body) = if let Node::FuncDefinition { name, kind, args, body } = node {
      (name.text.clone(), self.get_type(kind), args, body)
    } else { unreachable!() };

    if self.lookup(&name).is_some() {
      self.error(node, "symbol already exists", format!("{name} has already been defined."));
      return;
    }

    self.enter();

    let mut params = vec![];
    args.iter().for_each(|arg| {
      let (name, kind) = if let Expr::Argument { name, kind } = arg {
        (name.text.clone(), self.get_type(kind))
      } else { unreachable!() };

      params.push(kind.clone());
      self.insert(name, Symbol::Variable { kind, mutable: true });
    });

    let body = if let Node::Compound { body } = *body.clone()
      { body } else 
      { vec![*body.clone()] };

    let mut has_emmission = false;
    for node in body.clone() {
      if let Node::ValueEmission { expr } = node.clone() {
        let ret = self.get_type(&expr);
        if ret != kind {
          self.error(&node, "mismatched types", format!("{name} has been declared to return a {kind}, but is returning {ret} here."))
        }
        has_emmission = true;
      } else {
        self.check_node(&node);
      }
    }

    if kind != Type::NullVoid && !has_emmission {
      self.error(body.last().unwrap(), "mismatched types", format!("expected to emit a {kind}, nothing was emmitted."));
    }

    self.leave();

    self.insert(name, Symbol::Function { kind, args: params })
  }
  fn add_type(&mut self, node: &Node) {
    let (name, fields) = if let Node::DeclareObject { name, fields } = node {
      (name.text.clone(), fields)
    } else { unreachable!() };

    let mut attrs: HashMap<String, Type> = HashMap::new();
    fields.iter().for_each(|field| {
      attrs.insert(field.0.to_string(), self.get_type(&field.1.1));
    });

    self.types.insert(name, Type::Struct { fields: attrs });
  }
  fn check_compound(&mut self, body: &Node) {
    let body = if let Node::Compound { body } = body.clone()
      { body } else 
      { vec![body.clone()] };

    body.iter().for_each(|node| self.check_node(node));
  }

  fn check_expression(&mut self, expr: &Expr) {
    match expr {
      Expr::VarRef { value } => {
        let var = self.lookup(&value.text);

        if var.is_none() {
          self.error(expr, "symbol not found", format!("{} is not defined within the current scope", value.text));
        };
      },
      Expr::Lambda { emit, body, .. } => {
        let body = if let Node::Compound { body } = *body.clone()
          { body } else { vec![*body.clone()] };

        let emit = self.get_type(&emit);
        
        let mut has_emmission = false;
        self.enter();
        body.iter().for_each(|node| {
          if let Node::ValueEmission { expr } = node {
            let kind = self.get_type(expr);

            if emit != kind {
              self.error(node, "mismatched types", format!("expected a value of type {emit}, but found {kind}"))
            }

            has_emmission = true;
          }
          self.check_node(node);
        });
        self.leave();

        if emit != Type::NullVoid && !has_emmission {
          self.error(expr, "mismatched types", format!("expected to emit a {emit}, nothing was emmitted."))
        }
      },
      Expr::Array { items } => {
        if items.len() == 0 { return; }
        let kind = self.get_type(&items[0]);

        for i in 1..items.len() {
          if self.get_type(&items[i]) != kind {
            self.error(expr, "mismatched types", format!("cannot have an array with elements of different types"))
          }
        }
      },
      Expr::ArrItem { parent, index } => {
        let index = self.get_type(index);
        if index != Type::Number {
          self.error(expr, "mismatched types", format!("cannot index an expression by a {index}, must be a number"));
          return;
        }

        let array = self.get_type(parent);

        match array {
          Type::String | Type::Array { .. } => (),
          _ => {
            self.error(&**parent, "invalid expression", format!("the type {} is not indexable", array));
          },
        };
      },
      Expr::MathOper { lhs, rhs, oper } => {
        let oper_t = oper.text.as_str();

        let l_t = self.get_type(&lhs);
        if ![Type::String, Type::Number].has(&l_t) {
          self.error(&**lhs, "invalid operation", format!("Cannot perform a binary operation on a {l_t}."));
        }

        let r_t = self.get_type(&rhs);
        if ![Type::String, Type::Number].has(&r_t) {
          self.error(&**lhs, "invalid operation", format!("Cannot perform a binary operation on a {r_t}."));
        }

        if l_t == Type::String && !["+", "+="].has(&oper_t) {
          self.error(oper, "invalid operation", format!("Cannot perform the action '{oper_t}' onto a {l_t}."))
        }
      },
      Expr::BoolOper { lhs, rhs, .. } => {
        let lhs = self.get_type(lhs);
        let rhs = self.get_type(rhs);

        if lhs != rhs {
          self.warn(expr, "mismatched types", format!("This will always be false because {lhs} is not comperable to {rhs}."))
        }
      },
      Expr::FunCall { name, args } => {
        let symbol = if let Some(symbol) = self.lookup(&name.text) {
          symbol
        } else {
          self.error(expr, "symbol not in scope", format!("{} has not been defined in the current scope.", &name.text));
          return;
        };

        let name = name.text.clone();

        if let Symbol::Function { args: params, .. } = symbol {
          if params.len() != args.len() {
            self.error(expr, "invalid arguments", format!("{name} was declared to have {} args, but is being called with {}.", params.len(), args.len()))
          }

          for i in 0..args.len() {
            let a = self.get_type(&args[i]);
            let p = params[i].clone();

            if a != p {
              self.error(&args[i], "mismatched types", format!("{name}'s {i}th should be of type {p}, not {a}."))
            }
          }
        }
      },
      Expr::Wrapper { expr } => self.check_expression(expr),
      Expr::TypeRef { base, .. } => {
        if self.types.get(&base.text).is_none() {
          self.error(base, "unknown type", format!("{} has not been declared within scope.", base.text))
        }
      },
      Expr::Attribute { parent, field } => {
        let name = if let Expr::VarRef { value } = &**field {
          value.text.clone()
        } else if let Expr::FunCall { name, .. } = &**field {
          name.text.clone()
        } else { unreachable!() };

        let obj = if let Expr::VarRef { value } = &**parent {
          value.text.clone()
        } else {
          self.error(&**parent, "not an object", "cannot find a struct with this name.");
          return;
        };

        if let Some(Symbol::Variable { kind, .. }) = self.lookup(&obj) {
          if let Type::Struct { fields } = kind {
            if fields.get(&name).is_none() {
              self.error(&**field, "Field does not exist", format!("{name} does not exist on {obj}"))
            }
          } else {
            self.error(&**parent, "symbol is not a struct", "Does not have accessable attributes.");
          }
        } else {
          self.error(&**parent, "symbol not in scope", "Has not been declared within scope.");
        };
      },
      _ => ()
    }
  }
}