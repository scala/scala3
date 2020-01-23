import scala.quoted._

enum Num {
  case One
  case Two
}

inline def foo(inline num: Num): Int = ${ fooExpr(num) }

private def fooExpr(num: Num) with QuoteContext : Expr[Int] = Expr(toInt(num))

private def toInt(num: Num): Int = num match {
  case Num.One => 1
  case Num.Two => 2
}
