object Test extends App {
  import compiletime.erasedValue

  inline def findImplied[T] given (ev: (T | Null) = null): Option[ev.type] = inline ev match {
    case _: Null => None
    case _ => Some(ev)
  }

  class A
  implied a for A

  class B

  class C[T]
  implied c for C[String]

  assert(!findImplied[A].isEmpty)
  assert(findImplied[B].isEmpty)

  assert(!findImplied[C[String]].isEmpty)
  assert(findImplied[C[Int]].isEmpty)

  findImplied[C[_]] match {
    case Some(c: C[t]) => // current GADT reasoning is not strong enough to infer `t == String`
      //val x: t = "a"; assert(x == "a")
    case None =>
      assert(false)
  }
}