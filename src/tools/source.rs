use std::{
   fmt::Display, iter, ops::Index, str::from_utf8_unchecked as from_bytes
};

use crate::tools::{bounds::Bounds, report::LogLevel, utils::*};

pub type Data = Vec<u8>;
pub type Rows = Vec<usize>;

pub struct Source {
   data: Data,
   name: String,
   rows: Rows,
   size: usize,
}

impl Source {
   pub fn init(name: String, data: Vec<u8>) -> Source {
      let size = data.len();

      let rows = iter::once(0).chain(data.iter().enumerate()
         .filter_map(|(i, ch)| (*ch == b'\n').then_some(i + 1)))
         .collect::<Vec<_>>();

      let data = data.into_iter().chain(iter::repeat_n(0, 8))
         .collect::<Vec<_>>();

      return Source { data, name, rows, size };
   }

   pub fn from(filename: String) -> Source {
      let data = match std::fs::read(&filename) {
         Ok(data) => data,
         Err(err) => {
            Self::panic(format_args!("could not read file {filename:?}.\n --> {err}"))
         }
      };

      return Self::init(filename, data);
   }

   pub fn name(&self) -> &String {
      return &self.name;
   }

   pub fn size(&self) -> usize {
      return self.size;
   }

   pub fn rows(&self) -> &Rows {
      return &self.rows;
   }
}

impl<I> Index<I> for Source where Data:Index<I> {
   type Output = <Data as Index<I>>::Output;
   
   fn index(&self, index: I) -> &Self::Output {
      return &self.data[index];
   }
}

impl Source {
   pub fn read(&self, start: usize, stop: usize) -> &str {
      unsafe { from_bytes(&self[start..=stop]) }
   }

   pub fn peak(&self, index: usize) -> char {
      return self.data[index] as char;
   }

   fn chunks(&self, bounds: Bounds) -> Vec<&str> {
      let [from, until] = bounds.lines(&self);

      let from = self.rows[from];
      let until = self.rows[until] - 1;

      return self.read(from, until).strip_suffix("\n").unwrap()
         .split("\n").collect::<Vec<_>>();
   }
}


impl Source {
   pub fn report<X:Display>(&self, level: LogLevel, name: &str, reason: &str, info: X, bounds: Bounds) {
      let [row, col] = bounds.local(&self);

      let rbuf = " ".repeat(row.to_string().len());
      let cbuf = " ".repeat(col - 1);
      let focus = "~".repeat(bounds.span() - 1);
      
      let context = self.chunks(bounds)
         .join(&format!("{rbuf} | "));

      echo!(
         "[{name}] {level}: {reason}";
         " >> {}[{row}:{col}]", self.name;
         "{rbuf} |";
         "{row} | {context}";
         "{rbuf} | {cbuf}^{focus} {info}";
      );
   }
}



