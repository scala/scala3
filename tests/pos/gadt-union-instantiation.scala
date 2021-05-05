object Test {
  trait C
  trait D extends C
  trait E extends C
  trait F extends C
  trait G extends C

  sealed trait Expr[T]
  case class FooExpr() extends Expr[D | E]

  object Test {
    def foo[T](x: Expr[T]): T = x match {
      case x: FooExpr =>
        val d : D | E = new D {}
        val t : T = d
        val d1: D | E = t
        d
    }

    val x: D | E = foo(FooExpr())
  }
}
