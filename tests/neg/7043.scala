object Test {
  type StrHead[X <: Tuple] = X match {
    case (x <: String) *: ? => x // error
  }

  // Futher minimized
  type M[X] = X match {
    case (x) *: ? => Int
  }

  // The exception can also be reached with normal pattern matching
  1 match {
    case _: Option[(x)] => ()
  }
}
