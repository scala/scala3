trait x0[T] { self: x0 => }           // error

trait x1[T] { self: (=> String) => }  // error

trait x2[T] { self: ([X] =>> X) => }   // error

