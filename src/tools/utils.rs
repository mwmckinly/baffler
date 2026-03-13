use std::fmt::Display;

pub trait Message: Sized {
   fn print<X:Display>(msg: X) {
      println!("[{}]: {msg}", Self::name());
   }
   fn panic<X:Display>(msg: X) -> ! {
      panic!("[{}]: {msg}", Self::name());
   }

   fn name<'a>() -> &'a str {
      std::any::type_name::<Self>().rsplit("::")
         .next().unwrap_or("")
   }

   fn parent<'a>(num: usize) -> &'a str {
      std::any::type_name::<Self>().rsplit("::")
         .nth(num).unwrap()
   }
}

impl<T> Message for T where T:Sized {}


macro_rules! prettify {
   ($text:expr, $code:expr) => {
      format_args!("\x1b[{}m{}\x1b[0m", $code, $text)
   };
}


macro_rules! echo {
   ($($fmt:literal $(, $args:expr)*);+ $(;)?) => {{
      use std::io::Write;
      let mut stderr = std::io::stderr().lock();
      $( writeln!(stderr, $fmt $(, $args)*).unwrap(); )+
   }};
}


