trait Expr { type T }
case class Inv[X](x: X) extends Expr { type T = X }
case class Inv2[X](x: X) extends Expr { type T >: X }

def eval(e: Expr): e.T = e match
  case Inv(x) => x
  case Inv2(x) => x

trait Foo[-T]
case class Bar() extends Foo[Int]

def foo(e1: Expr, e2: Foo[e1.T]) = e1 match {
  case Inv2(x) => e2 match {
    case Bar() =>
      val t0: Int = x
      val t1: e1.T = x
      val t2: Int = t1
  }
}

