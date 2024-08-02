
use std::{collections::HashMap, fmt::Display};

use serde::Serialize;

use crate::{parser::Parser, syntax::{Expr, Node}, util::{Has, Color, SourceInfo}};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum IRType {
  String, 
  Number, 
  Boolean, 
  NullVoid,
  Array { of: Box<IRType> },
  Struct { fields: HashMap<String, IRType> },
  None,
}
impl Display for IRType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().strip_suffix('\n').unwrap())
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum Symbol {
  Function { kind: IRType, args: Vec<IRType> },
  Variable { kind: IRType, mutable: bool },
  TypeRef { parent: IRType },
}
impl Symbol {
  pub fn new_func(kind: IRType, args: Vec<IRType>) -> Symbol {
    Self::Function { kind, args }
  }
  pub fn new_var(kind: IRType, mutable: bool) -> Symbol {
    Self::Variable { kind, mutable }
  }
  pub fn new_type(parent: IRType) -> Symbol {
    Self::TypeRef { parent }
  }
}
impl Display for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", serde_yaml::to_string(self).unwrap().strip_suffix('\n').unwrap())
  }
}

#[derive(Debug, Clone)]
struct Scope { 
  symbols: HashMap<String, Symbol>,
  parent: Option<Box<Scope>>
}
impl Scope {
  pub fn new(parent: Scope) -> Scope {
    Scope { symbols: HashMap::new(), parent: Some(Box::new(parent)) }
  }
  pub fn get<S:ToString>(&self, name: S) -> Option<&Symbol> {
    let symbol = self.symbols.get(&name.to_string());

    if symbol.is_some() {
      return symbol;
    }

    if self.parent.is_some() {
      return self.parent.as_ref().unwrap().get(name);
    }

    return None;
  }
  pub fn add<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.symbols.insert(name.to_string(), symbol);
  }
}

#[allow(non_snake_case)]
fn RootScope() -> Scope {
  let symbols = vec![
    ("str", IRType::String),
    ("num", IRType::Number),
    ("bool", IRType::Boolean),
    ("null", IRType::NullVoid)
  ].iter().map(|(name, kind)| {
    (name.to_string(), Symbol::new_type(kind.clone()))
  }).into_iter().collect();

  Scope { symbols, parent: None }
}

pub struct Anaylzer {
  source: Vec<Node>,
  scope: Scope,

  file: String,
  code: Vec<String>,
}

//: main / initialization functions
impl Anaylzer {
  pub fn init(parser: &mut Parser) -> Anaylzer {
    let root = RootScope();
    let file = parser.metadata();
    let tree = parser.parse();

    let tree = if let Node::Compound { body } = tree 
      { body } else { vec![tree] };

    Anaylzer {
      source: tree,
      scope: root,
      file: file.name(),
      code: file.lines(),
    }
  }
  pub fn analyze(&mut self) {
    for node in self.source.clone() {
      self.check_node(&node);
    }
  }
}

//: scope related functions
impl Anaylzer {
  fn enter(&mut self) {
    self.scope = Scope::new(self.scope.clone());
  }
  fn leave(&mut self) {
    if let Some(parent) = self.scope.parent.take() 
      { self.scope = *parent; } else { unreachable!() }
  }
  fn lookup<S:ToString>(&self, name: S) -> Option<&Symbol> {
    self.scope.get(name.to_string())
  }
  fn append<S:ToString>(&mut self, name: S, symbol: Symbol) {
    self.scope.add(name, symbol);
  }
}

impl Anaylzer {
  fn message<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, kind: String, header: V, message: S) {
    let (line, col, len) = node.info();

    let buffr = ' '.to_string().repeat(line.to_string().len());
    let space = ' '.to_string().repeat(col -1);
    let value = '~'.to_string().repeat(len - 1);

    let info = vec![
      format!("{}[{}:{}] --> {kind}: {header}", self.file, line, col),
      format!("{} | {}", line, self.code[line - 1].clone()),
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
  fn inform<V:Display, S:Display, Value:SourceInfo>(&self, node: &Value, header: V, message: S) {
    self.message(node, "info".color(33), header, message)
  }
}

//: expression validation
impl Anaylzer {
  fn get_type(&self, expr: &Expr) -> IRType {
    match expr {
      Expr::String { .. } => IRType::String,
      Expr::Number { .. } => IRType::Number,
      Expr::Boolean { .. } => IRType::Boolean,
      Expr::VarRef { value } => {
        let var = self.lookup(&value.text);
        let val = if let Some(v) = var {
          v
        } else {
          self.error(value, "symbol not found", format!("{} is not a defined symbol within scope.", value.text));
          return IRType::None;
        };

        match val {
          Symbol::Function { kind, .. } => return kind.clone(),
          Symbol::Variable { kind, .. } => return kind.clone(),
          Symbol::TypeRef { parent } => {
            return parent.clone()
          },
        }
      },
      Expr::Object { kind, .. } => {
        let obj = self.lookup(&kind.text);
        if obj.is_none() {
          self.error(kind, "symbol not found", format!("{} has not been defined within scope", kind.text));
        }

        match obj.unwrap() {
          Symbol::TypeRef { parent } => parent.clone(),
          _ => IRType::None,
        }
      },
      Expr::Array { items } => {
        if items.len() == 0 {
          return IRType::None;
        }

        let first = self.get_type(&items[0]);

        items.iter().for_each(|item| {
          if self.get_type(&item) != first {
            self.error(item, "mismatched types", format!("All items within an array must be of the same type."));
          }
        });

        return first;
      },
      Expr::ArrItem { parent, index } => {
        let arr_type = self.get_type(parent);
        let element = match arr_type {
          IRType::String => IRType::String,
          IRType::Array { of } => *of,
          _ => {
            self.error(&**parent, "invalid operation", format!("Cannot perform index onto a {arr_type}."));
            return IRType::None;
          },
        };

        let idx_type = self.get_type(index);

        if idx_type != IRType::Number {
          self.error(&**index, "mismatched types", format!("Cannot perform index using a {idx_type}"));
        };

        return element;
      },
      Expr::MathOper { lhs, rhs, oper } => {
        let lhs_t = self.get_type(lhs);
        let rhs_t = self.get_type(rhs);
        let oper = oper.text.as_str();

        let res = match lhs_t.clone() {
          IRType::String => IRType::String,
          IRType::Number => IRType::Number,
          IRType::Array { of } => IRType::Array { of },
          _ => {
            self.error(&**lhs, "invalid operation", format!("Cannot perform binary operation on a {lhs_t}"));
            return IRType::NullVoid;
          },
        };

        if lhs_t == IRType::String && !["+", "+="].has(&oper) {
          self.error(&**lhs, "invalid operation", format!("Cannot perform '{oper}' operation upon a {lhs_t}"));
        }

        //: appending items to arrays;
        if let IRType::Array { of: l } = &lhs_t {
          if let IRType::Array { of: r } = rhs_t {
            if r != l.clone() {
              self.error(expr, "mismatched types", format!("Cannot combine arrays of different types. {r}[] + {l}[]"));
              return IRType::NullVoid;
            }
            return lhs_t;
          }
          match rhs_t {
            _ if rhs_t == *l.clone() => {
              return *l.clone();
            },
            _ => {
              self.error(expr, "mismatched types", format!("Cannot append {rhs_t} onto a {l}[]."));
              return lhs_t;
            },
          }
        }

        return res;
      },
      Expr::BoolOper { lhs, rhs, .. } => {
        if self.get_type(lhs) != self.get_type(rhs) {
          self.warn(expr, "mismatched types", format!("Comparing different types will always result in a false emission."));
        };

        return IRType::Boolean
      },
      Expr::FunCall { name, args } => {
        let symbol = self.lookup(&name.text);
        let result = if let Some(sym) = symbol {
          sym
        } else {
          self.error(&name.clone(), "ssymbol not found", format!("{} does not exist within scope.", &name.text));
          return IRType::NullVoid;
        };

        let (ret, params) = if let Symbol::Function { kind, args } = result {
          (kind.clone(), args)
        } else {
          return IRType::NullVoid;
        };

        let args_t: Vec<IRType> = args.iter().map(|arg| {
          self.get_type(arg)
        }).collect();

        if args_t.len() != params.len() {
          self.error(expr, "arguments differ in length", format!("{} expected {} arguments but was given {}", &name.text, params.len(), args_t.len()));
          return ret;
        }

        for i in 0..args_t.len() {
          if args_t[i] != params[i] {
            self.error(&args[i], "mismatched types", format!("Was expecting a {}, but was given a {}", params[i], args_t[i]));
          }
        }

        return ret;
      },
      Expr::Wrapper { expr } => self.get_type(expr),
      Expr::Argument { kind, .. } => self.get_type(kind),
      Expr::TypeRef { base, arrays } => {
        let symbol = self.lookup(&base.text); 
        let symbol = if let Some(s) = symbol {
          s
        } else {
          self.error(base, "symbol not found", format!("{} could not be found within scope.", base.text));
          return IRType::NullVoid;
        };

        let mut kind: IRType = match symbol {
          Symbol::Function { kind, .. } => {
            self.error(base, "invalid expression", format!("Expected a type-ref, but found a function."));
            self.inform(base, format!("function emits {kind}"), format!("If you wanted to reference the emission type of {}, it is {}.", &base.text, kind));
            return IRType::None;
          },
          Symbol::Variable { kind, .. } => {
            self.error(base, "invalid expression", format!("Expected a type-ref, but found a variable."));
            self.inform(base, format!("variable is a {kind}"), format!("If you wanted to reference the same type as {}, it is {}.", &base.text, kind));
            return IRType::None;
          },
          Symbol::TypeRef { parent } => parent.clone(),
        };

        for _ in 0..*arrays {
          kind = IRType::Array { of: Box::new(kind) }
        }

        return kind;
      },
      Expr::TypeCast { from, to } => todo!("cannot cast as of now. {from} -> {to}"),
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
          return IRType::NullVoid;
        };

        let obj = self.lookup(&obj);
        let obj = if obj.is_some() {
          obj.unwrap()
        } else {
          self.error(&**parent, "symbol not in scope", "Has not been declared within scope.");
          return IRType::NullVoid;
        };

        let obj = if let Symbol::Variable { kind, .. } = obj {
          kind
        } else {
          self.error(&**parent, "symbol is not a struct", "Does not have accessable attributes.");
          return IRType::NullVoid;
        };

        let fields = if let IRType::Struct { fields } = obj {
          fields
        } else {
          self.error(&**parent, "symbol is not a struct", "Does not have accessable attributes.");
          return IRType::NullVoid;
        };

        let value = fields.get(&name);

        if let Some(v) = value {
          return v.clone();
        } else {
          self.error(&**parent, "symbol not found", format!("{} is not a member of {}", name, obj));
          return IRType::NullVoid;
        };

      },
      Expr::NullVoid => IRType::NullVoid,
    }
  }

  fn check_node(&mut self, node: &Node) {
    match node {
      Node::SetAssign { .. } => self.add_variable(node, false),
      Node::VarAssign { .. } => self.add_variable(node, true),
      Node::ModifyVar { .. } => self.modify_value(node),
      Node::FuncDefinition { .. } => self.add_function(node),
      Node::DeclareObject { .. } => self.add_type(node),
      Node::ObjExtention { .. } => self.extend_obj(node),

      Node::Expression { expr } => { self.get_type(expr); },

      _ => ()
    }
  }
  fn add_variable(&mut self, node: &Node, mutable: bool) {
    let (name, kind, value) = match node {
      Node::SetAssign { name, kind, value } => (name, kind, value),
      Node::VarAssign { name, kind, value } => (name, kind, value),
      _ => unreachable!(),
    };

    if self.lookup(&name.text).is_some() {
      self.error(name, "symbol already exists", format!("{} has already been declared.", &name.text));
      return;
    }

    let kind = if let Some(k) = kind 
      { self.get_type(k) } else 
      { self.get_type(value) };

    let v_ty = self.get_type(value);

    if kind != v_ty {
      self.error(node, "mismatched types", format!("{} was declared to be a {kind}, but assigned to {v_ty}.", &name.text));
    }

    self.append(name.text.clone(), Symbol::new_var(kind, mutable));
  }
  fn modify_value(&self, node: &Node) {
    let (name, value) = match node {
      Node::ModifyVar { name, value } => (name.text.clone(), self.get_type(value)),
      _ => unreachable!(),
    };

    let symbol = if let Some(val) = self.lookup(name.clone()) {
      val
    } else {
      self.error(node, "symbol does not exist", format!("{name} could not be found within scope."));
      return;
    };

    let (kind, mutable) = match symbol {
      Symbol::Variable { kind, mutable } => ( kind, mutable ),
      _ => {
        self.error(node, "invalid operation", format!("Cannot assign a value to a {symbol}"));
        return;
      },
    };

    if !mutable {
      self.error(node, "invalid operation", format!("{name} is a constant. You cannot change its value."));
      return;
    }

    if kind != &value {
      self.error(node, "mismatched types", format!("{name} was declared to be a {kind}, you cannot change its type."));
      return;
    }
  } 
  fn add_function(&mut self, node: &Node) {
    let (name, kind, args, body) = if let Node::FuncDefinition { name, kind, args, body } = node {
      let body = if let Node::Compound { body } = *body.clone() { body } else { vec![*body.clone()] };
      ( name.text.clone(), self.get_type(kind), args, body )
    } else { unreachable!() };

    if self.lookup(&name).is_some() {
      self.error(node, "symbol already exists", format!("{name} is already defiend within scope."));
      return;
    }

    self.enter();

    let args: Vec<IRType> = args.iter().map(|arg| {
      let (name, kind) = if let Expr::Argument { name, kind } = arg 
        { (name, self.get_type(kind)) } else { unreachable!() };
      self.scope.add(&name.text, Symbol::Variable { kind: kind.clone(), mutable: true });

      kind
    }).collect();

    let mut emits = false;

    for node in body {
      if let Node::ValueEmission { expr } = node.clone() {
        let emit = self.get_type(&expr);
        if emit != kind {
          self.error(&node, "mismatched types", format!("Expected to emit {kind}, but found {emit}."));
        } else { emits = true; }
      } else {
        self.check_node(&node);
      }
    }

    if !emits && kind != IRType::NullVoid {
      self.error(node, "mismatched types", format!("Expected to emit {kind}, but found Null."))
    }

    self.leave();

    self.scope.add(name, Symbol::new_func(kind, args));
  }
  fn add_type(&mut self, node: &Node) {
    let (name, fields) = if let Node::DeclareObject { name, fields } = node {
      ( name.text.clone(), fields.clone() )
    } else { unreachable!() };

    if self.lookup(&name).is_some() {
      self.error(node, "symbol already existts", format!("{name} already has already been defined within scope."));
      return;
    }

    let fields = fields.into_iter().map(|(name, kind)| {
      (name.text.clone(), self.get_type(&kind))
    }).collect();

    let object = IRType::Struct { fields };
    self.scope.add(name, Symbol::new_type(object))
  }
  fn extend_obj(&mut self, node: &Node) {
    let body = if let Node::Compound { body } = node.clone() 
      { body } else { vec![node.clone()] };

    for node in body {
      self.add_function(&node);
    }
  }
} 