import scala.quoted.*

inline def myMacro: Int = ${ myMacroExpr }

private def myMacroExpr(using Quotes): Expr[Int] =
  '{ def y[T](i: T): T = i; ${ identity('{ y[Int](1) }) } }
