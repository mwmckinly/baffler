use std::ops::Deref;

use crate::{token::token::Token, tools::{bounds::Bounds, source::Source}};

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
   fn token(&mut self, _: &mut Vec<Token>) {
      let head = self.index;
      let data = self.walk();

      match data {
         _ if data.is_whitespace() => { 
            while self.check().is_whitespace() 
               { self.index += 1; }
         },

         _ => {
            let e_msg = format!("{data:?} is not a valid character.");
            self.err("idk", e_msg, Bounds::from(head, self.index));
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
}

