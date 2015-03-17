// Illustrates a use case where we need hygiene.

object hygiene {

  class D[T]

  case class C[T](x: D[T])
// without hygiene, this gave
// 7: error: wrong number of type arguments for hygiene.C.D, should be 0
// 7: error: constructor C in class C does not take type parameters

  object C {
    class C
  }

  val c = C.apply(new D)

  c.x

}
