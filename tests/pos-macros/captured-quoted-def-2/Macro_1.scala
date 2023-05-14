import scala.quoted.*

inline def myMacro: Int = ${ myMacroExpr }

private def myMacroExpr(using Quotes): Expr[Int] =
  '{ def y(): Int = 1; ${ identity('{ y() }) } }
