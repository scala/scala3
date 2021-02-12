import scala.quoted.*

def test(using Quotes) = {

  given Quotes = ???

  implicit def IntIsToExpr: ToExpr[Int] = new {
    def apply(n: Int)(using Quotes) = n match {
      case Int.MinValue    => '{Int.MinValue}
      case _ if n < 0      => '{- ${apply(n)}}
      case 0               => '{0}
      case _ if n % 2 == 0 => '{ ${apply(n / 2)} * 2 }
      case _               => '{ ${apply(n / 2)} * 2 + 1 }
    }
  }

  implicit def BooleanToExpr: ToExpr[Boolean] = new {
    implicit def apply(b: Boolean)(using Quotes) =
      if (b) '{true} else '{false}
  }

  implicit def ListToExpr[T: ToExpr: Type]: ToExpr[List[T]] = new {
    def apply(xs: List[T])(using Quotes) = xs match {
      case x :: xs1 => '{ ${ Expr(x) } :: ${ apply(xs1) } }
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
  Expr(StringContext("a", "b", "c"))

  val xs: Expr[List[Int]] = Expr(1 :: 2 :: 3 :: Nil)
}
