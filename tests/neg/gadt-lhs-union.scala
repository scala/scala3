object A {
  sealed trait Expr[+T]
  final case class FooExpr() extends Expr[1 | 2]

  object Test {
    def foo[T](x: Expr[T]): T = x match {
      case x: FooExpr =>
        3 // error
    }

    val x: 1 | 2 = foo(FooExpr())
  }
}

object B {
  trait C
  trait D extends C
  trait E extends C
  trait F extends C
  trait G extends C

  sealed trait Expr[+T]
  final case class FooExpr[+S >: (D & E) | F]() extends Expr[S]

  object Test {
    def foo[T](x: Expr[T]): T = x match {
      case x: FooExpr[(D & E) | F] =>
        new D with E
    }

    val x: (D & E) | F = foo(FooExpr[(D & E) | F]())
  }
}
