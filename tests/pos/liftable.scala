import scala.quoted._

object Test {

  implicit def IntIsQuotable: Quotable[Int] = new {
    def toExpr(n: Int): Expr[Int] = n match {
      case Int.MinValue    => '(Int.MinValue)
      case _ if n < 0      => '(-(~toExpr(n)))
      case 0               => '(0)
      case _ if n % 2 == 0 => '( ~toExpr(n / 2) * 2)
      case _               => '( ~toExpr(n / 2) * 2 + 1)
    }
  }

  implicit def BooleanIsQuotable: Quotable[Boolean] = new {
    implicit def toExpr(b: Boolean) =
      if (b) '(true) else '(false)
  }

  implicit def ListIsQuotable[T: Quotable]: Quotable[List[T]] = new {
    def toExpr(xs: List[T]): Expr[List[T]] = xs match {
      case x :: xs1 => '{ ~implicitly[Quotable[T]].toExpr(x) :: ~toExpr(xs1) }
      case Nil => '(Nil: List[T])
    }
  }

  val xs: Expr[List[Int]] = 1 :: 2 :: 3 :: Nil
}
