use crate::tools::source::Source;

use crate::{
   tools::utils::*,
};



#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bounds {
   pub head: usize,
   pub tail: usize,
}

impl Bounds {
   pub fn from(head: usize, tail: usize) -> Bounds {
      if head == tail {
         let node = Self::parent(1);
         panic!("{node}: !!invalid indexing!!")
      }

      return Bounds { head, tail };
   }

   pub fn local(&self, src: &Source) -> [usize; 2] {
      let rows = src.rows();

      let row = match rows.binary_search(&self.head) 
         { Ok(r) => r, Err(r) => r - 1 };

      let col = self.head - rows[row];

      return [row + 1, col + 1];
   }

   pub fn lines(&self, src: &Source) -> [usize; 2] {
      let rows = src.rows();

      let head = match rows.binary_search(&self.head) 
         { Ok(r) => r, Err(r) => r - 1 };
      
      let tail = match rows.binary_search(&self.tail) 
         { Ok(r) => r, Err(r) => r - 1 };

      return [head, tail + 1];
   }

   pub fn span(&self) -> usize {
      return self.tail - self.head;
   }
}

impl From<(usize, usize)> for Bounds {
   fn from(value: (usize, usize)) -> Self {
      return Self { head: value.0, tail: value.1 };
   }
}

impl std::fmt::Display for Bounds {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      return write!(f, "bound:[{} >> {}]", self.head, self.tail);
   }
}


