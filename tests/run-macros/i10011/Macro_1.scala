import scala.quoted._

inline def printPos[T](inline expr: T): (Int, Int) =
  ${ printPos('expr) }

private def printPos[T](expr: Expr[T])(using QuoteContext): Expr[(Int, Int)] =
  val pos = expr.asReflectTree.pos
  Expr((pos.start, pos.end))
