

macro_rules! derives {
   ($item:item) => {
      #[derive(Clone, Copy, Debug, Eq, PartialEq)]
      $item
   };
}


