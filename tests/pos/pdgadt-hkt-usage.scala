object test {
  class Foo[A]
  class Inv { type F[_] }
  class InvFoo extends Inv { type F[x] = Foo[x] }

  object Test {
    def foo(x: Inv) = x match {
      case x: InvFoo =>
        val z1: x.F[Int] = ??? : Foo[Int]
        val z2: Foo[Int] = ??? : x.F[Int]
      case _ =>
    }
  }
}
