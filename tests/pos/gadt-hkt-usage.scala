object test {
  class Foo[A]
  class Inv[M[_]]
  class InvFoo extends Inv[Foo]

  object Test {
    def foo[F[_]](x: Inv[F]) = x match {
      case x: InvFoo =>
        val z: F[Int] = ??? : Foo[Int]
      case _ =>
    }
  }
}
