import scala.quoted._

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
      case x :: xs1 => '{ ${ Value(x) } :: ${ apply(xs1) } }
      case Nil => '{Nil: List[T]}
    }
  }

  Value(true)
  Value(1)
  Value('a')
  Value(1)
  Value(1)
  Value(1L)
  Value(1.0f)
  Value(1.0)
  Value("abc")
  Value(StringContext("a", "b", "c"))

  val xs: Expr[List[Int]] = Value(1 :: 2 :: 3 :: Nil)
}
