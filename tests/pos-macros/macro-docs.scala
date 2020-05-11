import scala.quoted._

object MacrosMD_Liftable {

  given (using s: Scope) as s.Liftable[Boolean] {
    def toExpr(b: Boolean) =
      if (b) '{ true } else '{ false }
  }

  given (using s: Scope) as s.Liftable[Int] {
    def toExpr(n: Int) = n match {
      case Int.MinValue    => '{ Int.MinValue }
      case _ if n < 0      => '{ - ${ toExpr(-n) } }
      case 0               => '{ 0 }
      case _ if n % 2 == 0 => '{ ${ toExpr(n / 2) } * 2 }
      case _               => '{ ${ toExpr(n / 2) } * 2 + 1 }
    }
  }

  given [T](using s: Scope)(using s.Liftable[T], s.Type[T]) as s.Liftable[List[T]] {
    def toExpr(xs: List[T]) = xs match {
      case head :: tail => '{ ${ s.Expr(head) } :: ${ toExpr(tail) } }
      case Nil => '{ Nil: List[T] }
    }
  }

  def showExpr[T](using s: Scope)(expr: s.Expr[T]): s.Expr[String] = {
    val code: String = expr.show
    Expr(code)
  }

}
