import scala.quoted.*

inline def printPos[T](inline expr: T): (Int, Int) =
  ${ printPos('expr) }

private def printPos[T](expr: Expr[T])(using Quotes): Expr[(Int, Int)] =
  import quotes.reflect.*
  val pos = expr.asTerm.pos
  Expr((pos.start, pos.end))
