type *:[A, B] = A match // error: Recursion limit exceeded.
  case (B *: x) => A
  case (x *: y) => x *: (B *: y)
  case _ => A *: B
