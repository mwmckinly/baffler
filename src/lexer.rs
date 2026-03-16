use std::ops::Deref;

use crate::{
   tools::{ bounds::*, source::* },
   token::{ class::*, token::* }
};

use crate::tools::utils::*;


pub struct Lexer {
   src: Source,
   index: usize,
}

impl Lexer {
   pub fn lex(src: Source) -> (Source, Vec<Token>) {
      let mut this = Lexer { src, index: 0 };
      let mut tokens = vec![];

      while this.index < this.size() 
         { this.token(&mut tokens); }

      return (this.src, tokens);
   }
}

impl Deref for Lexer {
   type Target = Source;

   fn deref(&self) -> &Self::Target {
      return &self.src;
   }
}

impl Lexer {
   fn token(&mut self, tokens: &mut Vec<Token>) {
      let head = self.index;
      let text = self.walk();

      let mut push = |lex: &Self, class: Class| {
         tokens.push(Token::init(class, lex.span(head)));
      };

      match text {
         _ if text.is_whitespace() => { 
            while self.check().is_whitespace() 
               { self.index += 1; }
         },

         '(' | '[' | '{' | ')' | ']' | '}' => {
            push(&self, Bracket::init(self.data(head)));
         }

         '+' | '-' | '*' | '/' | '%' => {
            if self.curr() == '=' { self.index += 1; }

            push(&self, Operator::init(self.data(head)));
         },

         '=' | '<' | '>' | '!' => {
            if self.curr() == '=' { self.index += 1; }

            push(&self, Operator::init(self.data(head)));
         },

         '|' | '&' => push(&self, Operator::init(self.data(head))),

         '0'..='9' | '.' => {
            let mut deci = text == '.';

            if deci && !matches!(self.curr(), '0'..='9') {
               push(&self, Class::Dot);
               return;
            }

            loop {
               match self.curr() {
                  '.' if deci => { break; },
                  '.' => { deci = true },
                  '0'..='9' | '_' => {  },
                  _ => break
               }

               self.index += 1;
            }

            push(&self, Literal::Number.into());
         },

         ';' => push(&self, Class::SemiColon),
         ':' => push(&self, Class::Colon),
         ',' => push(&self, Class::Comma),

         '"' => {
            loop {
               match self.curr() {
                  '"' => { self.index += 1; break; },
                  '\\' => self.index += 2,
                  _ => self.index += 1,
               }
            }

            push(&self, Literal::String.into());
         },

         '_' | _ if text.is_alphabetic() => {
            loop {
               let ch = self.curr();

               if ch != '_' && !ch.is_alphanumeric() 
                  { break; }
               
               self.index += 1;
            }

            let bytes = &self.src[head..self.index];

            let class = if let Some(cls) = Keyword::test(bytes) { cls } else 
                        if let Some(cls) = Literal::test(bytes) { cls } else
                        { Class::Identifier };

            push(&self, class);
         },

         '\0' => push(&self, Class::FileExit),

         _ => {
            let e_msg = format!("{text:?} is not a valid character.");
            self.err("idk", e_msg, self.span(head));
         },
      }
   }

   fn walk(&mut self) -> char {
      self.index += 1;
      return self.src.peak(self.index - 1);
   }

   fn check(&self) -> char {
      return self.src.peak(self.index + 1);
   }

   fn curr(&self) -> char {
      return self.src.peak(self.index);
   }

   fn span(&self, head: usize) -> Bounds {
      return Bounds { head, tail: self.index };
   }

   fn data(&self, head: usize) -> &[u8] {
      return &self.src[head..self.index];
   }
}

