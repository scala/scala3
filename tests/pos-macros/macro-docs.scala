import scala.quoted.*

object MacrosMD_ToExpr {

  given ToExpr[Boolean] {
    def apply(b: Boolean)(using Quotes) =
      if (b) '{ true } else '{ false }
  }

  given ToExpr[Int] {
    def apply(n: Int)(using Quotes) = n match {
      case Int.MinValue    => '{ Int.MinValue }
      case _ if n < 0      => '{ - ${ apply(-n) } }
      case 0               => '{ 0 }
      case _ if n % 2 == 0 => '{ ${ apply(n / 2) } * 2 }
      case _               => '{ ${ apply(n / 2) } * 2 + 1 }
    }
  }

  given [T: ToExpr : Type] => ToExpr[List[T]] {
    def apply(xs: List[T])(using Quotes) = xs match {
      case head :: tail => '{ ${ Expr(head) } :: ${ apply(tail) } }
      case Nil => '{ Nil: List[T] }
    }
  }

  def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] = {
    val code: String = expr.show
    Expr(code)
  }

}
