use std::borrow::Borrow as _;
use std::collections::HashMap;
use std::fmt::Display;
use ordermap::OrderMap;

use crate::utils::Wrapper;

#[derive(Clone, PartialEq, Eq)]
enum Type {
  String,
  Number,
  Boolean,
  None,

  Object(HashMap<String, Type>),
  Array(Box<Type>),
  Function(Vec<Type>, Box<Type>)
}

#[derive(Clone, PartialEq)]
enum Value {
  String { value: String },
  Number { value: f64 },
  Boolean { value: bool },
  None,

  Object { value: HashMap<String, Value> },
  Array { value: Vec<Value> },
  TypeRef { origin: Type },
  Function { args: OrderMap<String, Type>, emits: Type, code: Vec<Action> },
}

impl Value {
  pub fn kind(&self) -> Type {
    let kind = match self {
      Value::String { .. } => Type::String,
      Value::Number { .. } => Type::Number,
      Value::Boolean { .. } => Type::Boolean,
      Value::Array { value } => Type::Array(value[0].kind().wrap()),
      Value::None { .. } => Type::None,
      Value::Object { value } => {
        let value = value.into_iter().map(|(name, value)| {
          (name.to_string(), value.kind())
        }).collect::<HashMap<String, Type>>();

        Type::Object(value)
      },
      Value::TypeRef { origin } => origin.clone(),
      Value::Function { args, emits, .. } => {
        let args = args.clone().into_values().map(|x| x).collect::<Vec<Type>>();
        Type::Function(args, emits.clone().wrap())
      },
    };

    return kind;
  }
}
impl Display for Value {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Value::String { value } => value.to_string(),
      Value::Number { value } => value.to_string(),
      Value::Boolean { value } => value.to_string(),
      Value::None => "none".to_string(),
      Value::Object { value } => {
        let attrs = value.into_iter().map(|(name, value)| {
          format!("{name}: {value}")
        }).collect::<Vec<String>>().join(", ");

        format!("{{ {attrs} }}")
      },
      Value::Array { value } => {
        let value = value.into_iter().map(|value| {
          value.to_string()
        }).collect::<Vec<String>>().join(", ");
        format!("[{value}]")
      },
      Value::TypeRef { origin } => format!("<{origin}>"),
      Value::Function { args, emits, .. } => {
        let args = args.into_iter().map(|x| {
          x.1.to_string()
        }).collect::<Vec<String>>().join(", ");
        format!("({args}) -> {emits}")
      },
    };

    return write!(f, "{s}")
  }
}
impl Display for Type {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Type::String => "str".into(),
      Type::Number => "num".into(),
      Type::Boolean => "bool".into(),
      Type::None => "null".into(),
      Type::Object(attrs) => {
        let attrs = attrs.iter().map(|(name, value)| {
          format!("{name}: {value}")
        }).collect::<Vec<String>>().join(", ");

        format!("{{ {attrs} }}")
      },
      Type::Array(parent) => format!("{parent}[]"),
      Type::Function(args, emits) => {
        let args = args.iter().map(|kind| {
          kind.to_string()
        }).collect::<Vec<String>>().join(", ");

        format!("({args}) -> {emits}")
      }
    };

    write!(f, "{s}")
  }
}

#[derive(Clone, PartialEq)]
enum Action {
  Assign { name: String, value: Value, mutable: bool },
  Modify { name: String, value: Value },
  CallFn { name: String, args: Vec<Value> },
  Import { package: String },
  Return { value: Value },
  Object { name: String, fields: HashMap<String, Type> },
}

#[derive(Clone)]
enum Symbol {
  Variable { value: Value, mutable: bool },
  TypeRefr { parent: Type },
}

impl Symbol {
  pub fn var(value: Value, mutable: bool) -> Self {
    Self::Variable { value, mutable }
  }
  pub fn refr(parent: Type) -> Self {
    Self::TypeRefr { parent }
  }
}

impl Display for Symbol {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let s: String = match self {
      Symbol::Variable { value, mutable } => format!("sym:var {{ value: {value}, const: {} }}", !mutable),
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

