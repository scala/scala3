import scala.quoted._

def test(using QuoteContext) = {

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
      case x :: xs1 => '{ ${ Lifted(x) } :: ${ toExpr(xs1) } }
      case Nil => '{Nil: List[T]}
    }
  }

  Lifted(true)
  Lifted(1)
  Lifted('a')
  Lifted(1)
  Lifted(1)
  Lifted(1L)
  Lifted(1.0f)
  Lifted(1.0)
  Lifted("abc")

  val xs: Expr[List[Int]] = Lifted(1 :: 2 :: 3 :: Nil)
}
