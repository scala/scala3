import scala.quoted._

inline def printExpr[T](inline expr: T): Unit =
  ${ printExpr('expr) }

private def printExpr[T](expr: Expr[T])(using QuoteContext): Expr[Unit] =
  '{ println( ${Expr(expr.foo) } ) }
