import scala.quoted._

object MacrosMD_Liftable {

  given Liftable[Boolean] with {
    def toExpr(b: Boolean) =
      if (b) '{ true } else '{ false }
  }

  given Liftable[Int] with {
    def toExpr(n: Int) = n match {
      case Int.MinValue    => '{ Int.MinValue }
      case _ if n < 0      => '{ - ${ toExpr(-n) } }
      case 0               => '{ 0 }
      case _ if n % 2 == 0 => '{ ${ toExpr(n / 2) } * 2 }
      case _               => '{ ${ toExpr(n / 2) } * 2 + 1 }
    }
  }

  given [T: Liftable : Type]: Liftable[List[T]] with {
    def toExpr(xs: List[T]) = xs match {
      case head :: tail => '{ ${ Expr(head) } :: ${ toExpr(tail) } }
      case Nil => '{ Nil: List[T] }
    }
  }

  def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] = {
    val code: String = expr.show
    Expr(code)
  }

}
