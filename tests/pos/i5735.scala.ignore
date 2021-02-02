object Test {
  // The identity on types of the form N | P[a, P[b, ... P[z, N]]]
  type Nested[X, P[_, _], N] = X match {
    case N => N
    case P[a, b] => P[a, Nested[b, P, N]]
  }

  class Foo[T] { def apply[Y >: T <: Nested[T, Tuple2, Unit]](t: T): T = t }
  def foo[T] = new Foo[T]

  foo.apply((21, ()))
}
