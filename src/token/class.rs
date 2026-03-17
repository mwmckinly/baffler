use macro_rules_attribute::apply;

use crate::tools::utils::*;

#[apply(derives)]
pub enum Class {
   Identifier,
   Keyword(Keyword),
   Literal(Literal),

   Bracket(Bracket),
   Operator(Operator),

   Comma, Colon,
   SemiColon, Dot,
   FileExit,
}

#[apply(derives)]
pub enum Keyword {
   Fun,   Emit, 
   Set,    Var,
   If,    Else,
   Break, Loop,
   Import,
   Export,
}
classify!(Keyword, Keyword);

#[apply(derives)]
pub enum Literal {
   String,
   Number,
   Boolean,
   Null,
}
classify!(Literal, Literal);

#[apply(derives)]
pub enum Bracket {
   OpenCurly,   ExitCurly,
   OpenSquare, ExitSquare,
   OpenParen,   ExitParen,
}
classify!(Bracket, Bracket);

#[apply(derives)]
pub enum Operator {
   Add, AddSet,
   Sub, SubSet,
   Mul, MulSet,
   Div, DivSet,
   Mod, ModSet,
   SetEqual,

   Equ, Neq,
   Gt,  Gte,
   Lt,  Lte,
   And,  Or,

   Not, Neg,

   Range,
}

classify!(Operator, Operator);

pub trait Classy: Sized + Into<Class> {
   fn test(_data: &[u8]) -> Option<Class> {
      Self::panic("no direct conversion from text.")
   }
   fn init(data: &[u8]) -> Class {
      return match Self::test(data) {
         Some(cls) => cls,
         None => {
            let message = format_args!("{} cannot be build from {:?}", Self::name(), from_bytes(&data));
            Self::panic(message);
         },
      };
   }
}

impl Classy for Keyword {
   fn test(_data: &[u8]) -> Option<Class> {
      let text = from_bytes(&_data);

      let kind = match text {
         "fun" =>    Self::Fun,
         "use" =>    Self::Import,
         "set" =>    Self::Set,
         "var" =>    Self::Var,
         "if" =>     Self::If,
         "else" =>   Self::Else,
         "loop" =>   Self::Loop,
         "emit" =>   Self::Emit,
         "break" =>  Self::Break,
         _ => return None,
      };
      
      return Some(kind.into());
   }
}
impl Classy for Operator {
   fn test(data: &[u8]) -> Option<Class> {
      let kind = match data.len() {
         1 => match data[0] as char {
            '=' => Self::SetEqual,
            
            '+' => Self::Add,
            '-' => Self::Sub,
            '*' => Self::Mul,
            '/' => Self::Div,
            '%' => Self::Mod,

            '!' => Self::Not,
            '>' => Self::Gt,
            '<' => Self::Lt,
            
            '&' => Self::And,
            '|' => Self::Or,

            _ => return None,
         },
         2 => match data[0] as char {
            '=' => Self::Equ,
            
            '+' => Self::AddSet,
            '-' => Self::SubSet,
            '*' => Self::MulSet,
            '/' => Self::DivSet,
            '%' => Self::ModSet,

            '!' => Self::Neq,
            '>' => Self::Gte,
            '<' => Self::Lte,
            '.' => Self::Range,
            _ => return None,
         },
         _ => return None,
      };

      return Some(kind.into());
   }
}
impl Classy for Literal {
   fn test(data: &[u8]) -> Option<Class> {
      return matches!(from_bytes(&data), "true" | "false").then_some(Self::Boolean.into())
   }
}
impl Classy for Bracket {
   fn test(_data: &[u8]) -> Option<Class> {
      let kind = match _data[0] as char {
         '(' => Class::Bracket(Self::OpenParen), 
         '[' => Class::Bracket(Self::OpenSquare), 
         '{' => Class::Bracket(Self::OpenCurly), 
         ')' => Class::Bracket(Self::ExitParen), 
         ']' => Class::Bracket(Self::ExitSquare), 
         '}' => Class::Bracket(Self::ExitCurly), 
         _ => return None,
      };

      return Some(kind);
   }
}


impl std::fmt::Display for Class {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      let text = format!("{self:?}")
         .replace("(", ":")
         .replace(")", "")
         .to_lowercase();

      return write!(f, "{text}");
   }
}


