use std::{borrow::Borrow, collections::HashMap};
use std::fmt::{Display, Debug};
use serde::Serialize;

use crate::{logger::Logger, parser::Parser, syntax::{Expr, Node}, utils::{Coords, Wrapper}};

#[derive(Debug, Clone, Serialize, PartialEq)]
enum Type {
  String, Number, Boolean, NullVoid,
  Object { attrs: HashMap<String, Type> },
  Function { kind: Box<Type>, args: Vec<Type> },
  Array { kind: Box<Type> },
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let v = match self {
      Type::String => "str",
      Type::Number => "num",
      Type::Boolean => "bool",
      Type::NullVoid => "null",
      Type::Object { .. } => "object",
      Type::Function { kind, args } => {
        let args: Vec<String> = args.iter().map(|x| x.to_string()).collect();
        &format!("func<{}> -> {}", (args.as_slice()).join(", "), kind)
      }
      Type::Array { kind } => &format!("{}[]", kind),
    }; write!(f, "{v}")
  }
}

#[derive(Clone)]
enum Symbol {
  Variable { mutable: bool, kind: Type },
  TypeRef { kind: Type },
}

#[derive(Clone)]
struct Scope {
  symbols: HashMap<String, Symbol>,
  parent: Option<Box<Scope>>,
}
impl Scope {
  pub fn new(parent: Scope) -> Scope {
    let child = Scope {
      symbols: HashMap::new(),
      parent: Some(parent.wrap())
    };

    return child;
  }
  pub fn get<S:ToString>(&self, name: S) -> Option<&Symbol> {
    if let Some(symbol) = self.symbols.get(&name.to_string()) {
      return Some(symbol);
    }

    if let Some(parent) = self.parent.borrow() {
      return parent.get(name.to_string());
    } else { return None; };
  }
  pub fn add<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.symbols.insert(name.to_string(), symbol);
  }
}

#[allow(non_snake_case)]
fn RootScope() -> Scope {
  let mut types: HashMap<String, Symbol> = vec![
    ("str", Type::String),
    ("num", Type::Number),
    ("bool", Type::Boolean),
    ("null", Type::NullVoid),
  ].into_iter().map(|(name, kind)| {
    ( name.to_string(), Symbol::TypeRef { kind })
  }).collect();

  let symbols: HashMap<String, Symbol> = vec![
    ("disp", Type::Function { kind: Type::NullVoid.wrap(), args: vec![
      Type::String
    ] })
  ].iter().map(|(x, y)| {
    ( x.to_string(), Symbol::Variable { mutable: false, kind: y.clone() } )
  }).collect();

  types.extend(symbols);

  return Scope { symbols: types, parent: None }
}

pub struct Analyzer {
  scope: Scope,
  logger: Box<Logger>,
  nodes: Vec<Node>
}

impl Analyzer {
  fn enter(&mut self) {
    self.scope = Scope::new(self.scope.clone());
  }
  fn leave(&mut self) {
    if let Some(parent) = self.scope.parent.take() {
      self.scope = *parent;
    }
  }
}
impl Analyzer {
  pub fn init(parser: Parser) -> Self {
    let (nodes, logger) = parser.parse();
    return Self {
      scope: RootScope(),
      logger, nodes,
    };
  }
  pub fn analyze(mut self) {
    for node in self.nodes.clone()
      { self.evaluate(&node); }
  }
}
impl Analyzer {
  pub fn error<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) {
    println!("{}", self.logger.error(header, message.to_string(), spot));
  }
  pub fn inform<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) {
    println!("{}", self.logger.error(header, message.to_string(), spot));
  }
  pub fn warn<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) {
    println!("{}", self.logger.error(header, message.to_string(), spot));
  }
  
  fn lookup<S:ToString>(&self, name: S) -> Option<&Symbol> {
    return self.scope.get(name);
  }
  fn append<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.scope.add(name, symbol);
  }
}
impl Analyzer {
  fn evaluate(&mut self, node: &Node) -> Type {
    match node {
      Node::SetAssign { .. } => self.eval_assignment(node, false),
      Node::VarAssign { .. } => self.eval_assignment(node, true),
      Node::ChangeVal { .. } => self.eval_modification(node),
      Node::ImportLib { .. } => self.eval_import(node),
      Node::DeclareType { .. } => self.eval_object(node),
      Node::Compound { .. } => return self.eval_compound(node),

      Node::EmitValue { value } => return self.eval_expression(value),
      Node::Expression { expr } => return self.eval_expression(expr),
    }
    return Type::NullVoid;
  }

  fn eval_assignment(&mut self, node: &Node, mutable: bool) {
    let (name, kind) = match node {
      Node::SetAssign { name, value } => ( name.clone(), self.eval_expression(value) ),
      Node::VarAssign { name, value } => ( name.clone(), self.eval_expression(value) ),
      _ => unreachable!()
    };

    if self.lookup(&name.text).is_some() {
      self.error("symbol already exists", format!("{:?} has already been declared", &name.text), &name);
      return;
    }

    self.append(name.text, Symbol::Variable { mutable, kind });
  }
  fn eval_modification(&mut self, node: &Node) {
    let (name, value) = if let Node::ChangeVal { name, value } = node {
      (name.clone(), self.eval_expression(value))
    } else { unreachable!() };

    let res = self.lookup(&name.text);
    let res = if let Some(r) = res { r } else {
      self.error("symbol does not exist", format!("{} could not be resolved", &name.text), &name);
      return;
    };

    let (mutable, kind) = match res {
      Symbol::Variable { mutable, kind } => (mutable, kind),
      Symbol::TypeRef { .. } => {
        self.error("invalid operation", "cannot modify state of a typeref.", &name);
        return;
      },
    };

    if !mutable {
      self.error("invalid operation", "cannot modify value of a constant.", &name);
      return;
    }

    if &value != kind {
      self.error("invalid operation", format!("{} should be assigned to {kind}, not {value}", &name.text), &name);
    }
  }
  fn eval_import(&mut self, node: &Node) {
    let path = if let Node::ImportLib { path } = node.clone()
      { path.clone() } else { unreachable!() };

    let path_s = path.iter().map(|x| {
      x.text.clone()
    }).collect::<Vec<String>>().join("/") + ".ori";

    let res = if let Ok(bool) = std::fs::exists(&path_s)
      { bool } else { false };

    let res = if !res {
      if let Ok(bool) = std::fs::exists("lib/".to_string()+&path_s) { bool } else { false }
    } else { res };

    if !res {
      self.error("invalid path", format!("{path_s} is not a valid filepath."), path.as_slice());
    }
  }
  fn eval_object(&mut self, node: &Node) {
    let (name, attrs) = if let Node::DeclareType { name, attrs } = node 
      { (name.clone(), attrs.clone()) } else { unreachable!() };

    if self.lookup(&name.text).is_some() {
      self.error("symbol already exists", format!("{} has already been declared", &name.text), &name);
      return;
    }

    let symbol = Type::Object { attrs: attrs.iter().map(|attr| {
      let (name, kind) = if let Expr::TypePair { name, kind } = attr {
        ( name.text.clone(), self.eval_expression(kind) )
      } else { unreachable!() };

      (name, kind)
    }).collect() };

    self.append(name, Symbol::TypeRef { kind: symbol });
  }
  fn eval_compound(&mut self, node: &Node) -> Type {
    let body = if let Node::Compound { value } = node 
      { value } else { unreachable!() };

    let mut kind = Type::NullVoid;

    for node in body {
      if let Node::EmitValue { value } = node {
        kind = self.eval_expression(value)
      }
      self.evaluate(node); 
    }

    return kind;
  }
  
  fn eval_expression(&mut self, expr: &Expr) -> Type {
    let kind = match expr {
      Expr::String { .. } => Type::String,
      Expr::Number { .. } => Type::Number,
      Expr::Boolean { .. } => Type::Boolean,
      Expr::VarRef { value } => {
        let res = if let Some(var) = self.lookup(&value.text) { var } else { 
          self.error("symbol does not exist", format!("{:?} could not be resolved within scope.", value.text), expr);
          return Type::NullVoid;
        };

        match res {
          Symbol::Variable { kind, .. } => kind,
          Symbol::TypeRef { kind } => kind,
        }.clone()
      },
      Expr::Object { attrs } => {
        let fields = attrs.iter().map(|attr| {
          if let Expr::TypePair { name, kind } = attr {
            ( name.text.clone(), self.eval_expression(kind) )
          } else { unreachable!() }
        }).collect::<HashMap<String, Type>>();

        let mut found = false;

        for symbol in self.scope.symbols.iter() {
          let kind = if let Symbol::TypeRef { kind } = symbol.1 
            { kind } else { continue; };

          let attrs = if let Type::Object { attrs } = kind 
            { attrs } else { continue; };

          if attrs == &fields { found = true; }
        }

        if !found {
          let fields = fields.iter().map(|(name, kind)| {
            format!("{name}: {kind}")
          }).collect::<Vec<String>>().join(", "); 
          self.error("dynamic object", format!("There is not type with these fields {{ {fields} }}."), expr)
        }

        Type::Object { attrs: fields }
      },
      Expr::FunCall { name, args } => {
        let res = if let Some(symbol) = self.lookup(&name.text) { symbol } else {
          self.error("symbol does not exist", format!("{:?} could not be resolved within scope.", name.text), name);
          return Type::NullVoid;
        };
        
        let kind = match res {
          Symbol::Variable { kind, .. } => kind,
          Symbol::TypeRef { .. } => {
            self.error("invalid expression", format!("{:?} is not a function, its a type.", name.text), expr);
            return Type::NullVoid;
          }
        };

        let (kind, params) = match kind {
          Type::Function { kind, args } => {
            (kind.clone(), args.clone())
          }
          _ => {
            self.error("invalid expression", format!("{} is not a function, its a {}.", name.text, kind), expr);
            return Type::NullVoid;
          },
        };

        if params.len() != args.len() {
          self.error("invalid expression", format!("{} expected {} args, but was given {}", name.text, params.len(), args.len()), expr);
          return Type::NullVoid;
        }

        for i in 0..args.len() {
          let arg_t = self.eval_expression(&args[i].clone());
          if params[i] != arg_t {
            self.error("mismatched types", format!("{} expected {} as its {i}th, but was given {}", name.text, params[i], arg_t), &args[i]);
          }
        }

        return kind.as_ref().clone();
      },
      Expr::Array { value } => {
        if value.len() == 0 { return Type::Array { kind: Type::NullVoid.wrap() }; }

        let first = self.eval_expression(&value[0]);

        value.iter().for_each(|expr| {
          let kind = self.eval_expression(expr);
          if kind != first {
            self.error("mismatched types", format!("expected {first}, but was given {kind}. All elements within an array must be of the same type."), expr);
          }
        });

        return Type::Array { kind: first.wrap() }
      },
      Expr::Index { parent, index } => {
        let of = self.eval_expression(parent);
        let kind = match of {
          Type::String => Type::String,
          Type::Array { kind } => *kind,
          _ => {
            self.error("invalid operation", format!("Cannot perform Index upon {of}."), expr);
            return Type::NullVoid;
          },
        };

        let i_kind = self.eval_expression(index);

        if i_kind != Type::Number {
          self.error("invalid operation", format!("Cannot perform Index with {i_kind}, must be a number"), expr);
        };

        return kind;
      },
      Expr::Lambda { args, kind, body } => {
        self.enter();
        
        let params = args.iter().map(|arg| {
          let (name, kind) = if let Expr::TypePair { name, kind } = arg {
            (name.text.clone(), self.eval_expression(kind))
          } else { unreachable!() };
          self.append(name, Symbol::Variable { mutable: true, kind: kind.clone() });
          kind
        }).collect();

        let body = if let Node::Compound { value } = *body.clone() 
          { value } else { vec![*body.clone()] };

        body.iter().for_each(|node| { self.evaluate(node); });

        self.leave();

        return Type::Function { kind: self.eval_expression(kind).wrap(), args: params };
      },
      Expr::IfExpr { cond, body, other } => {
        if self.eval_expression(cond) != Type::Boolean {
          self.error("invalid expression", format!("does not evaluate to a boolean."), expr);
          return Type::NullVoid;
        }
        let body_t = self.evaluate(body);
        let other_t = self.evaluate(other);

        if body_t != other_t {
          self.error("invalid expression", format!("expected {body_t}, but was given {other_t}."), expr);
        }

        return body_t;
      },
      Expr::BoolOper { lhs, oper: _, rhs } => {
        let lhs_t = self.eval_expression(lhs);
        let rhs_t = self.eval_expression(rhs);

        if lhs_t != rhs_t {
          self.warn("mismatched types", format!("will always emit false because {lhs_t} and {rhs_t} arent compatable."), expr);
        }

        return Type::Boolean;
      },
      Expr::MathOper { lhs, oper, rhs } => {
        let lhs_t = self.eval_expression(lhs);
        let rhs_t = self.eval_expression(rhs);
        let oper_s = &oper.text;

        if lhs_t != Type::String && lhs_t != rhs_t {
          self.error("invalid operation", format!("cannot perform binary operation upon a {lhs_t} with a {rhs_t}."), expr);
          return lhs_t;
        }

        if lhs_t == Type::String && (oper_s != "+" && oper_s != "+=") {
          self.error("invalid operation", format!("cannot perform {oper_s:?} operation upon a string."), expr);
          return lhs_t;
        }
        
        let kind = match &lhs_t {
          Type::String 
            | Type::Number
            | Type::Array { .. }
            => lhs_t,
          _ => {
            self.error("invalid operation", format!("cannot perform binary operation upon a {lhs_t}"), expr);
            return lhs_t;
          },
        };

        return kind;
      },
      Expr::Chained { lhs, rhs, .. } => {
        let lhs = self.eval_expression(lhs);
        let rhs = self.eval_expression(rhs);
        if lhs != Type::Boolean || rhs != Type::Boolean {
          self.error("invalid expression", format!("cannot join {lhs} and {rhs} using a logical operatior."), expr);
        }
        return Type::Boolean;
      },
      Expr::TypeRef { base, arrs } => {
        let res = self.lookup(&base.text);
        let res = if let Some(r) = res { r } else {
          self.error("symbol does not exist", format!("{} could not be resolved within scope.", base.text), expr);
          return Type::NullVoid;
        };
        let mut base = if let Symbol::TypeRef { kind } = res.clone() { kind } else {
          self.error("invalid expression", "must reference a type, not a variable", base);
          return Type::NullVoid;
        };

        for _ in 0..*arrs { base = Type::Array { kind: base.wrap() } }

        return base;
      },
      Expr::TypePair { kind, .. } => self.eval_expression(kind),
      Expr::NullVoid { .. } => Type::NullVoid,
    };

    return kind;
  }
}

