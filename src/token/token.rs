use crate::{
   token::class::Class, 
   tools::{
      bounds::Bounds, source::Source
   }
};



#[derive(Debug, Clone, Copy)]
pub struct Token {
   bounds: Bounds,
   class: Class,   
}

impl Token {
   pub fn init(class: Class, bounds: Bounds) -> Token {
      return Token { bounds, class };
   }

   pub fn cls(&self) -> &Class {
      return &self.class;
   }

   pub fn bnd(&self) -> &Bounds {
      return &self.bounds;
   }

   pub fn txt<'a>(&self, src: &'a Source) -> &'a str {
      return src.read(self.bounds.head, self.bounds.tail);
   }

   pub fn disp<'a>(&'a self, src: &'a Source) {
      println!("{{\n  class: {},\n  bounds: {},\n  text: {:?}\n}}", self.class, self.bounds, self.txt(src))
   }
}


