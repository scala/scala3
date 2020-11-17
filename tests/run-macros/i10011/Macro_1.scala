import scala.quoted._

inline def printPos[T](inline expr: T): (Int, Int) =
  ${ printPos('expr) }

private def printPos[T](expr: Expr[T])(using QuoteContext): Expr[(Int, Int)] =
  import qctx.reflect._
  val pos = Term.of(expr).pos
  Expr((pos.start, pos.end))
