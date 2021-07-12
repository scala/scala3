type *:[A, B] = A match
  case (B *: x) => A
  case (x *: y) => x *: (B *: y)
  case _ => A *: B
