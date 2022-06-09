import scala.quoted.*

inline def myMacro: Int = ${ myMacroExpr }

private def myMacroExpr(using Quotes): Expr[Int] =
  '{ def y(i: Int)(j: Int, k: Int): Int = 1; ${ identity('{ y(1)(2, 3) }) } }
