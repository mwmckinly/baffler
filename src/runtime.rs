use std::borrow::Borrow as _;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::Write;
use serde::Serialize;

use crate::logger::Logger;
use crate::parser::Parser;
use crate::syntax::{Expr, Node};
use crate::token::Token;
use crate::utils::{Color as _, Coords, Wrapper};

type PrimeFunc = fn(&Box<Logger>, Vec<Value>, &Vec<Expr>) -> Value;

#[derive(Clone, PartialEq, Eq, Serialize)]
pub enum Type {
  String,
  Number,
  Boolean,
  NullVoid,

  Object(HashMap<String, Type>),
  Array(Box<Type>),
}

#[derive(Clone, PartialEq, Serialize)]
pub enum Value {
  String(String),
  Number(f64),
  Boolean(bool),
  NullVoid,

  Object(HashMap<String, Value>),
  Array(Vec<Value>),
  TypeRef(Type),
}

impl Value {
  pub fn as_type(&self) -> Type {
    match self {
      Value::String(_) => Type::String,
      Value::Number(_) => Type::Number,
      Value::Boolean(_) => Type::Boolean,
      Value::NullVoid => Type::NullVoid,
      Value::Object(attrs) => {
        let attrs = attrs.iter().map(|(name, value)| {
          (name.to_string(), value.as_type())
        }).collect();

        Type::Object(attrs)
      },
      Value::Array(items) => {
        let parent = items[0].as_type();
        Type::Array(parent.wrap())
      },
      Value::TypeRef(t) => t.clone(),
    }
  }
}

impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Value::String(value) => value.to_string(),
      Value::Number(value) => value.to_string(),
      Value::Boolean(value) => value.to_string(),
      Value::NullVoid => "null".to_string(),
      Value::Object(attrs) => {
        let attrs = attrs.iter().map(|(name, value)| {
          format!("{name}: {value}")
        }).collect::<Vec<String>>().join(", ");

        format!("{{ {attrs} }}")
      },
      Value::Array(items) => {
        let items = items.iter().map(|item| {
          item.to_string()
        }).collect::<Vec<String>>().join(", ");

        format!("[{items}]")
      },
      Value::TypeRef(t) => t.to_string(),
    };

    write!(f, "{s}")
  }
}

impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Type::String => "str".into(),
      Type::Number => "num".into(),
      Type::Boolean => "bool".into(),
      Type::NullVoid => "null".into(),
      Type::Object(attrs) => {
        let attrs = attrs.iter().map(|(name, value)| {
          format!("{name}: {value}")
        }).collect::<Vec<String>>().join(", ");

        format!("{{ {attrs} }}")
      },
      Type::Array(parent) => format!("{parent}[]"),
    };

    write!(f, "{s}")
  }
}

#[derive(Clone, Serialize)]
enum Symbol {
  Variable { value: Value, mutable: bool },
  Function { args: HashMap<String, Type>, emmission: Type, code: Vec<Node> },
  TypeRefr { parent: Type },
}

impl Symbol {
  pub fn var(value: Value, mutable: bool) -> Self {
    Self::Variable { value, mutable }
  }
  pub fn func(args: HashMap<String, Type>, emmission: Type, code: Box<Node>) -> Self {
    let code = if let Node::Compound { value } = *code 
      { value } else { vec![*code] };
    Self::Function { args, emmission, code }
  }
  pub fn refr(parent: Type) -> Self {
    Self::TypeRefr { parent }
  }
}

impl Display for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Symbol::Variable { value, mutable } => format!("sym:var {{ value: {value}, const: {} }}", !mutable),
      Symbol::Function { args, emmission, .. } => {
        let args = args.iter().map(|(_, kind)| {
          kind.to_string()
        }).collect::<Vec<String>>().join(", ");

        format!("sym:func {{ args: {args}, emits: {emmission} }}")
      }
      Symbol::TypeRefr { parent } => format!("sym:type {{ parent: {parent} }}"),
    };

    write!(f, "{s}")
  }
}


#[derive(Clone)]
struct Scope {
  symbols: HashMap<String, Symbol>,
  parent: Option<Box<Scope>>
}

impl Scope {
  pub fn init(parent: Scope) -> Scope {
    let symbols = HashMap::new();

    return Scope { parent: Some(parent.wrap()), symbols };
  }

  pub fn get<S:ToString>(&self, name: S) -> Option<&Symbol> {
    if let Some(symbol) = self.symbols.get(&name.to_string()) {
      return Some(symbol);
    }

    if let Some(parent) = self.parent.borrow() {
      return parent.get(name.to_string());
    } else { return None; };
  }

  pub fn set<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.symbols.insert(name.to_string(), symbol);
  }
}
impl Display for Scope {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let string = self.symbols.iter().map(|(name, value)| {
      format!("{{ name: {name}, symbol: {value} }}")
    }).collect::<Vec<String>>().join("\n");

    write!(f, "symbols: \n{string}")
  }
}

#[allow(non_snake_case)]
fn RootScope() -> Scope {
  let symbols = vec![
    ("str", Symbol::refr(Type::String)),
    ("num", Symbol::refr(Type::Number)),
    ("bool", Symbol::refr(Type::Boolean)),
    ("null", Symbol::refr(Type::NullVoid)),
  ].into_iter().map(|(x, y)| {
    (x.to_string(), y)
  }).collect();

  return Scope {
    symbols, parent: None
  };
}
#[allow(non_snake_case, unused)]
pub fn RuntimePrimatives() -> HashMap<String, PrimeFunc>{
  let funcs: Vec<(&str, PrimeFunc)> = vec![
    ("disp", |_: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
      let args = args.iter().map(|x| {
        x.to_string()
      }).collect::<Vec<String>>();

      println!("{}", args.join(", "));

      Value::NullVoid
    }),
    ("input", |_: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
      let msg = if let Some(first) = args.first() 
        { first.to_string() } else { "".into() };
      print!("{msg}");

      std::io::stdout().flush().unwrap();
      let mut input = String::new();

      if std::io::stdin().read_line(&mut input).is_err() {
        println!("{}: could not read input!", "error".color(31))
      };

      return Value::String(input);
    }),
    ("format", | logger: &Box<Logger>, args: Vec<Value>, exprs: &Vec<Expr>| -> Value {
      let msg = if let Some(str) = args.first() { str } else {
        logger.error("invalid arguments", "format takes in a string", &exprs[0]);
        return Value::NullVoid;
      };

      let msg = if let Value::String(val) = msg { val.to_string() } else {
        logger.error("invalid arguments", "format takes in a string", &exprs[0]);
        return Value::NullVoid;
      };

      if args.len() == 1 {
        return Value::String(msg);
      }

      let msg: Vec<&str> = msg.split("{}").collect();
      let mut str: Vec<String> = vec![];

      for i in 0..msg.len() - 1 {
        str.push(format!("{}{}", msg[i], args[i + 1]));
      };

      str.push(msg.last().unwrap().to_string());
      return Value::String(str.join(""));
    }),
    ("typeof", | _: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
      let args = args.into_iter().map(|x| { 
        Value::TypeRef(x.as_type())
      }).collect::<Vec<Value>>();

      return Value::Array(args);
    }),
  ];

  let funcs = funcs.into_iter().map(|(name, func)| {
    (name.to_string(), func)
  }).collect::<HashMap<String, PrimeFunc>>();

  return funcs;
}

pub struct Runtime {
  scope: Scope,
  nodes: Vec<Node>,
  logger: Box<Logger>,
}

impl Runtime {
  pub fn init(parser: Parser) -> Self {
    let (nodes, logger) = parser.parse();
    let scope = RootScope();

    return Self { scope, nodes, logger };
  }
  pub fn interperate(mut self) {
    self.nodes.clone().into_iter().for_each(|x| {
      self.compute(x);
    });
  }

  fn lookup<S:ToString>(&self, name: S) -> Option<&Symbol> {
    return self.scope.get(name.to_string());
  }
  fn insert<S:ToString>(&mut self, name: S, symbol: Symbol) {
    return self.scope.set(name, symbol);
  }

  fn enter(&mut self) {
    self.scope = Scope::init(self.scope.clone());
  }
  fn leave(&mut self) {
    if let Some(parent) = self.scope.parent.take() {
      self.scope = *parent;
    }
  }

  pub fn error<S:ToString, V:ToString, C:Coords>(&self, header: S, message: V, spot: C) {
    println!("{}", self.logger.error(header, message.to_string(), spot));
  }
}

impl Runtime {
  fn evaluate(&mut self, expr: Expr) -> Value {
    let value: Value = match expr.clone() {
      Expr::String { value } => Value::String(value.text),
      Expr::Number { value } => Value::Number(value.text.parse().unwrap()),
      Expr::Boolean { value } => Value::Boolean(value.text.parse().unwrap()),
      Expr::VarRef { value } => {
        let res = if let Some(sym) = self.lookup(&value.text) { sym } else {
          self.error("symbol does not exist", format!("{:?} could not be resolved", value.text), value);
          return Value::NullVoid;
        };


        match res {
          Symbol::Variable { value, .. } => value.clone(),
          Symbol::Function { .. } => {
            self.insert(value.text, res.clone());
            Value::NullVoid
          },
          Symbol::TypeRefr { .. } => {
            self.error("invalid reference", format!("{:?} is a type, not a value.", value.text), value);
            Value::NullVoid
          },
        }
      },
      Expr::FunCall { name, args } => {
        let args = match self.is_default_function(&name.text, args) {
          Ok(value) => return value,
          Err(args) => args,
        };

        let res = if let Some(symbol) = self.lookup(&name.text) { symbol } else {
          self.error("symbol does not exist", format!("{:?} could not be resolved", name.text), name);
          return Value::NullVoid;
        };

        let (params, emits, code) = match res {
          Symbol::Function { args, emmission, code } => ( args.clone(), emmission.clone(), code.clone() ),
          _ => {
            self.error("invalid operation", format!("{:?} is not a function", &name.text), name);
            return Value::NullVoid
          },
        };

        if &params.len() != &args.len() {
          self.error("arguments differ in length", format!("{:?} expected {} args, but was given {}.", &name.text, params.len(), args.len()), name);
          return Value::NullVoid;
        }

        let pars: Vec<&Type> = params.values().collect();
        let pnms: Vec<&String> = params.keys().collect();

        self.enter();

        for i in 0..args.len() {
          let x = &args[i];
          let y = self.evaluate(x.clone());

          if pars[i] != &y.as_type() {
            self.error("mismatched types", format!("{:?} expected {}, but was given {}.", &name.text, pars[i], y.as_type()), name);
            return Value::NullVoid;
          }

          self.insert(pnms[i], Symbol::Variable { value: y, mutable: true });
        }

        let emmission = self.run(Node::Compound { value: code });

        self.leave();

        if emmission.as_type() != emits {
          self.error("mismatched types", format!("{:?} expected to emit {}, but emits {}.", &name.text, emits, emmission.as_type()), name);
        }

        emmission
      },
      Expr::Object { attrs } => {
        let mut fields = HashMap::new();
        
        for attr in attrs {

          let (name, value) = if let Expr::ObjectField { name, attr } = attr {
            (name.text, self.evaluate(*attr))
          } else if let Expr::NullVoid { .. } = attr {
            return Value::Object(fields);
          } else { unreachable!() };
          
          fields.insert(name, value);
        }

        Value::Object(fields)
      },
      Expr::ObjectField { attr, .. } => self.evaluate(*attr),
      Expr::Array { value } => {
        let first = self.evaluate(value[0].clone()).as_type();
        
        let items = value.iter().map(|expr| {
          let item = self.evaluate(expr.clone());
          let kind = item.as_type();
          
          if kind != first {
            self.error("mismatched types", format!("found {kind} in {first}[]."), expr);
          }

          item
        }).collect::<Vec<Value>>();

        Value::Array(items)
      },
      Expr::Index { parent, index } => {
        let from = self.evaluate(*parent);
        let index = *index;

        let indx = if let Value::Number(num) = self.evaluate(index.clone()) {
          if num.fract() == 0.0 && num >= 0.0 { num as usize } else {
            self.error("invalid expression", format!("cannot perform index with a non positive integer."), index);
            return Value::NullVoid;
          }
        } else {
          self.error("invalid expression", format!("cannot perform index with a non positive integer."), index);
          return Value::NullVoid;
        };
        

        match from {
          Value::String(value) => {
            if indx >= value.len() {
              self.error("invalid expression", format!("index out of bounds of parent."), &expr);
              return Value::NullVoid;
            }

            Value::String((value.as_bytes()[indx] as char).to_string())
          },
          Value::Array(value) => {
            if indx >= value.len() {
              self.error("invalid expression", format!("index out of bounds of parent."), &expr);
              return Value::NullVoid;
            }
            return value[indx].clone();
          },
          _ => {
            self.error("invalid operation", format!("cannot perform indedx upon {}", from.as_type()), &expr);
            Value::NullVoid
          },
        }
      },
      Expr::Lambda { .. } => { Value::NullVoid },
      Expr::IfExpr { cond, body, other } => {
        let e = self.evaluate(*cond.clone());

        let condition: bool = match e {
          Value::String(value) => value.len() != 0,
          Value::Number(value) => value >= 0.0,
          Value::Boolean(value) => value,
          Value::NullVoid => false,
          _ => {
            self.error("invalid expression", format!("{} cannot be evaluated to a boolean.", e.as_type()), &*cond);
            false
          },
        };

        if condition { self.compute(*body) } else { self.compute(*other) }
      },
      Expr::BoolOper { lhs, oper, rhs } => {
        let l = self.evaluate(*lhs);
        let r = self.evaluate(*rhs);
        let o = oper.text.as_str();

        if o == "==" { return Value::Boolean(l == r) }
        if o == "!=" { return Value::Boolean(r != l) }

        let l: f64 = match l {
          Value::String(value) => value.len() as f64,
          Value::Number(value) => value,
          Value::Array(value) => value.len() as f64,
          _ => {
            self.error("invalid operation", format!("{o:?} is a numeric exclusive comparison operator."), oper);
            return Value::Boolean(false);
          },
        };
        let r: f64 = match r {
          Value::String(value) => value.len() as f64,
          Value::Number(value) => value,
          Value::Array(value) => value.len() as f64,
          _ => {
            self.error("invalid operation", format!("{o:?} is a numeric exclusive comparison operator."), oper);
            return Value::Boolean(false);
          },
        };

        let res: bool = match o {
          "<=" => l <= r,
          ">=" => l >= r,
          "<" => l < r,
          ">" => l > r,
          _ => unreachable!(),
        };

        return Value::Boolean(res);
      },
      Expr::MathOper { lhs, oper, rhs } => {
        let l = self.evaluate(*lhs.clone());
        let r = self.evaluate(*rhs);
        let o = oper.text.clone();

        match l.as_type() {
          Type::String 
            | Type::Number 
            | Type::Array(_) => (),
          _ => {
            self.error("invalid operation", format!("cannot perform {o:?} upon a {}", l.as_type()), &*lhs);
            return l;
          },
        }

        if o == "+" || o == "+=" {
          match l.clone() {
            Value::String(value) => {
              return Value::String(value + &r.to_string());
            },
            Value::Number(value) => {
              if let Value::Number(num) = r {
                return Value::Number(value + num);
              } else {
                self.error("invalid operation", format!("cannot perform {o:?} upon a {} with a {}.", l.as_type(), r.as_type()), &*lhs);
                return Value::NullVoid;
              }
            },
            Value::Array(value) => {
              match r.clone() {
                Value::Array(of) => {
                  if r.as_type() == l.as_type() {
                    return Value::Array([value, of].concat());
                  } else {
                    self.error("invalid operation", format!("cannot perform {o:?} upon a {} with a {}.", l.as_type(), r.as_type()), &*lhs);
                    return Value::NullVoid;
                  }
                },
                _ => {
                  if value[0].as_type() == Type::NullVoid {
                    return Value::Array(vec![r]);
                  } else if value[0].as_type() == r.as_type() {
                    return Value::Array(vec![value, vec![r]].concat());
                  } else {
                    self.error("invalid operation", format!("cannot perform {o:?} upon a {} with a {}.", l.as_type(), r.as_type()), &*lhs);
                    return Value::NullVoid;
                  }
                },
              }
            },
            _ => unreachable!()
          }
        }

        let r = if let Value::Number(num) = r { num } else {
          self.error("invalid operation", format!("{o:?} is an exclusive numeric operation."), &*lhs);
          return l;
        };

        let l = if let Value::Number(num) = l { num } else {
          self.error("invalid operation", format!("{o:?} is an exclusive numeric operation."), &*lhs);
          return l;
        };

        let res = match oper.text.as_str() {
          "-" | "-=" => l - r,
          "*" | "*=" => l * r,
          "/" | "/=" => l / r,
          "%" | "%=" => l % r,
          _ => unreachable!(),
        };

        return Value::Number(res);
      },
      Expr::Chained { lhs, stich, rhs } => {
        let l = self.evaluate(*lhs);
        let r = self.evaluate(*rhs);

        let l = if let Value::Boolean(val) = l { val } else {
          self.error("invalid operation", format!("cannot chain non-boolean values."), expr);
          return Value::Boolean(false);
        };
        let r = if let Value::Boolean(val) = r { val } else {
          self.error("invalid operation", format!("cannot chain non-boolean values."), expr);
          return Value::Boolean(false);
        };

        let s = stich.text.as_str();

        let res: bool = match s {
          "|" => l || r,
          "&" => l && r,
          _ => unreachable!()
        };

        return Value::Boolean(res);
      },
      Expr::TypeRef { base, arrs } => {
        let res = if let Some(symbol) = self.lookup(&base.text) { symbol } else {
          self.error("symbol does not exist", format!("{:?} could not be resolved", base.text), base);
          return Value::NullVoid;
        };

        let mut parent = if let Symbol::TypeRefr { parent } = res { parent.clone() } else {
          self.error("invalid reference", format!("{:?} is not a type", base.text), base);
          return Value::NullVoid;
        };

        for _ in 0..arrs {
          parent = Type::Array(parent.wrap())
        }

        return Value::TypeRef(parent);
      },
      _ => Value::NullVoid
    };

    return value;
  }
  fn is_default_function(&mut self, name: &String, args: Vec<Expr>) -> Result<Value, Vec<Expr>> {
    let funcs: Vec<(&str, PrimeFunc)> = vec![
      ("disp", |_: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
        let args = args.iter().map(|x| {
          x.to_string()
        }).collect::<Vec<String>>();

        println!("{}", args.join(", "));

        Value::NullVoid
      }),
      ("input", |_: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
        let msg = if let Some(first) = args.first() 
          { first.to_string() } else { "".into() };
        print!("{msg}");

        std::io::stdout().flush().unwrap();
        let mut input = String::new();

        if std::io::stdin().read_line(&mut input).is_err() {
          println!("{}: could not read input!", "error".color(31))
        };

        return Value::String(input);
      }),
      ("format", | logger: &Box<Logger>, args: Vec<Value>, exprs: &Vec<Expr>| -> Value {
        let msg = if let Some(str) = args.first() { str } else {
          logger.error("invalid arguments", "format takes in a string", &exprs[0]);
          return Value::NullVoid;
        };

        let msg = if let Value::String(val) = msg { val.to_string() } else {
          logger.error("invalid arguments", "format takes in a string", &exprs[0]);
          return Value::NullVoid;
        };

        if args.len() == 1 {
          return Value::String(msg);
        }

        let msg: Vec<&str> = msg.split("{}").collect();
        let mut str: Vec<String> = vec![];

        for i in 0..msg.len() - 1 {
          str.push(format!("{}{}", msg[i], args[i + 1]));
        };

        str.push(msg.last().unwrap().to_string());
        return Value::String(str.join(""));
      }),
      ("typeof", | _: &Box<Logger>, args: Vec<Value>, _: &Vec<Expr>| -> Value {
        let args = args.into_iter().map(|x| { 
          Value::TypeRef(x.as_type())
        }).collect::<Vec<Value>>();

        return Value::Array(args);
      }),
    ];

    let funcs = funcs.into_iter().map(|(name, func)| {
      (name.to_string(), func)
    }).collect::<HashMap<String, PrimeFunc>>();

    
    if let Some(func) = funcs.get(name) {
      let pargs: Vec<Value> = args.iter().map(
        |x| self.evaluate(x.clone()
      )).collect();
      return Ok(func(&self.logger, pargs, &args));
    } else {
      return Err(args);
    };
  }
}

impl Runtime {
  fn compute(&mut self, node: Node) -> Value {
    let mut emmission = Value::NullVoid;

    match &node {
      Node::SetAssign { .. } => self.assign(node), 
      Node::VarAssign { .. } => self.assign(node),
      Node::ChangeVal { .. } => self.modify(node),
      Node::ImportLib { .. } => self.import(node),
      Node::EmitValue { .. } => emmission = self.emit(node),
      Node::DeclareType { .. } => self.create_type(node),
      Node::Compound { .. } => emmission = self.run(node),
      Node::Expression { .. } => emmission = self.expression(node),
    };

    return emmission;
  }

  fn assign(&mut self, node: Node) {
    let (name, value, mutable) = match node {
      Node::SetAssign { name, value } => {
        let value = if let Expr::Lambda { args, kind, body } = value {
          self.fundef(name, args, kind, body); return;
        } else { self.evaluate(value) };

        (name, value, false)
      },
      Node::VarAssign { name, value } => {
        let value = if let Expr::Lambda { args, kind, body } = value {
          self.fundef(name, args, kind, body); return;
        } else { self.evaluate(value) };
        
        (name, value, true)
      },
      _ => unreachable!()
    };

    if self.lookup(&name.text).is_some() {
      self.error("symbol already exists", format!("{:?} has already been defined.", name.text), name);
      return;
    }

    self.insert(name.text, Symbol::var(value, mutable));
  }
  fn fundef(&mut self, name: Token, args: Vec<Expr>, kind: Box<Expr>, body: Box<Node>) {
    if self.lookup(&name.text).is_some() {
      self.error("symbol already exists", format!("{:?} has already been defined.", name.text), name);
      return;
    }

    let params = args.into_iter().map(|expr| {
      let (name, kind) = if let Expr::TypePair { name, kind } = expr {
        (name, kind.clone())
      } else { unreachable!() };
      let kind = if let Value::TypeRef(t) = self.evaluate(*kind) 
        { t } else { unreachable!() };

      (name.text.clone(), kind)
    }).collect::<HashMap<String, Type>>();

    let kind = match self.evaluate(*kind) {
      Value::TypeRef(t) => t,
      Value::NullVoid => Type::NullVoid,
      _ => unreachable!()
    };
    
    self.insert(name.text, Symbol::func(params, kind, body));
  }
  fn modify(&mut self, node: Node) {
    let (name, value) = if let Node::ChangeVal { name, value } = node {
      (name, self.evaluate(value))
    } else { unreachable!() };

    let symbol = if let Some(res) = self.lookup(&name.text) { res } else {
      self.error("symbol does not exist", format!("{:?} has could not be resolved.", name.text), name);
      return;
    };

    let (kind, mutable) = match symbol {
      Symbol::Variable { value, mutable } => (value.as_type(), mutable),
      Symbol::Function { .. } => {
        self.error("invalid operation", format!("{:?} is a function which cannot be assigned to a value.", name.text), name);
        return;
      },
      Symbol::TypeRefr { .. } => {
        self.error("invalid operation", format!("{:?} is a type reference which cannot be assigned to a value.", name.text), name);
        return;
      },
    };

    if !mutable {
      self.error("invalid operation", format!("{:?} is a constant and cannot be reassigned.", name.text), name);
      return;
    }

    if kind != value.as_type() {
      self.error("invalid operation", format!("{:?} has been assigned to be {kind}, not {}", name.text, value.as_type()), name);
      return;
    }

    self.insert(name, Symbol::var(value, true));
  }
  fn import(&mut self, node: Node) {
    let path = if let Node::ImportLib { path } = node.clone()
    { path.clone() } else { unreachable!() };

    let path_s = path.iter().map(|x| {
      x.text.clone()
    }).collect::<Vec<String>>().join("/") + ".baf";

    let res = if let Ok(bool) = std::fs::exists(&path_s)
      { bool } else { false };

    let res = if !res {
      if let Ok(bool) = std::fs::exists("lib/".to_string()+&path_s) { bool } else { false }
    } else { res };

    if !res {
      self.error("invalid path", format!("{path_s} is not a valid filepath."), path.as_slice());
    }
  }
  
  fn emit(&mut self, node: Node) -> Value {
    let expr = if let Node::EmitValue { value } = node 
      { value } else { unreachable!() };

    return self.evaluate(expr);
  }
  fn expression(&mut self, node: Node) -> Value {
    let expr = if let Node::Expression { expr } = node 
    { expr } else { unreachable!() };

    return self.evaluate(expr);
  }
  
  fn create_type(&mut self, node: Node) {
    let (name, attrs) = if let Node::DeclareType { name, attrs } = node {
      let attrs = attrs.into_iter().map(|expr| {
        if let Expr::TypePair { name, kind } = expr {
          let val = if let Value::TypeRef(t) = self.evaluate(*kind) 
            { t } else { unreachable!() };

          (name.text, val)
        } else { unreachable!() }
      }).collect::<HashMap<String, Type>>();

      (name, attrs)
    } else { unreachable!() };

    self.insert(name, Symbol::TypeRefr { parent: Type::Object(attrs) })
  }
  fn run(&mut self, node: Node) -> Value {
    let code = if let Node::Compound { value } = node 
      { value } else { vec![node] };

    let mut kind = Value::NullVoid;

    code.into_iter().for_each(|x| {
      if let Node::EmitValue { .. } = &x 
        { kind = self.compute(x); } else 
        { self.compute(x); }
    });

    return kind;
  }
}

