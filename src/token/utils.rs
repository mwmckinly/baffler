

macro_rules! derives {
   ($item:item) => {
      #[derive(Clone, Copy, Debug, Eq, PartialEq)]
      $item
   };
}


macro_rules! classify {
   ($t:ty, $variant:ident) => {
      impl From<$t> for Class {
         fn from(value: $t) -> Self {
            return Class::$variant(value);
         }
      }
   };
}


