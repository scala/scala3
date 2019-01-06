object Test2 {
  // The identity on types of the form N | P[a, P[b, ... P[z, N]]]
  type Nested[X, P[_, _], N] = X match {
    case N => N
    case P[a, b] => P[a, Nested[b, P, N]]
  }

  // Type bound:
  //   Recursion limit exceeded.
  //   Maybe there is an illegal cyclic reference?
  def p1[T <: Nested[T, Tuple2, Unit]](t: T): T = t
  //p1(())
  p1((23, 24))            // error
  p1(("foo", (23, 24)))   // error
}
