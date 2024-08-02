#![allow(unused)]

use std::collections::HashMap;
use crate::checker::Type;

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
  fn ir_type(&self) -> Type {
    match self {
      IRValue::String { .. } => Type::String,
      IRValue::Number { .. } => Type::Number,
      IRValue::Boolean { .. } => Type::Boolean,
      IRValue::Array { value } => Type::Array { of: Box::new(value[0].ir_type()) },
      IRValue::Struct { fields } => {
        let mut parts = HashMap::new();

        for (name, field) in fields {
          parts.insert(name.clone(), field.ir_type());
        }

        Type::Struct { fields: parts }
      },
      IRValue::Null => todo!(),
    }
  }
}

#[derive(Clone, PartialEq, Debug)]
pub enum IRNode {
  AssignVariable { name: String, value: IRValue, mutable: bool },
  CallFunction { name: String, args: Vec<IRValue> },
  DeclareFunction { name: String, args: HashMap<String, Type>, kind: Type, body: Box<IRNode> },

  EvaluateBinaryExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 0: +, 1: -, 2: *, 3: /, 4: %
  EvaluateBooleanExp { lhs: IRValue, rhs: IRValue, oper: usize }, // 10: ==, 11: <, 12: >, 13: <=, 14: >=, 15: !=

  ReturnValue { value: IRValue },
  Compound { body: Vec<IRNode> },
}