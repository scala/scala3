import scala.quoted._

def test(given QuoteContext) = {

  given QuoteContext = ???

  implicit def IntIsLiftable: Liftable[Int] = new {
    def toExpr(n: Int) = n match {
      case Int.MinValue    => '{Int.MinValue}
      case _ if n < 0      => '{- ${toExpr(n)}}
      case 0               => '{0}
      case _ if n % 2 == 0 => '{ ${toExpr(n / 2)} * 2 }
      case _               => '{ ${toExpr(n / 2)} * 2 + 1 }
    }
  }

  implicit def BooleanIsLiftable: Liftable[Boolean] = new {
    implicit def toExpr(b: Boolean) =
      if (b) '{true} else '{false}
  }

  implicit def ListIsLiftable[T: Liftable: Type]: Liftable[List[T]] = new {
    def toExpr(xs: List[T]) = xs match {
      case x :: xs1 => '{ ${ Expr(x) } :: ${ toExpr(xs1) } }
      case Nil => '{Nil: List[T]}
    }
  }

  Expr(true)
  Expr(1)
  Expr('a')
  Expr(1)
  Expr(1)
  Expr(1L)
  Expr(1.0f)
  Expr(1.0)
  Expr("abc")

  val xs: Expr[List[Int]] = Expr(1 :: 2 :: 3 :: Nil)
}
