object Test1 {
  // The identity on Unit
  type IdUnit[X] = X match {
    case Unit => Unit
  }

  // Type bound:
  //   Recursion limit exceeded.
  //   Maybe there is an illegal cyclic reference?
  def p1[T <: IdUnit[T]](t: T): T = t
  p1(())

  // Type constraint: OK
  def p2[T](t: T)(implicit ev: T <:< IdUnit[T]): T = t
  p2(())
}

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
  p1(())
  p1((23, ()))
  p1(("foo", (23, ())))

  // Type constraint: OK
  def p2[T](t: T)(erased implicit ev: T <:< Nested[T, Tuple2, Unit]): T = t
  p2(())
  p2((23, ()))
  p2(("foo", (23, ())))
}
