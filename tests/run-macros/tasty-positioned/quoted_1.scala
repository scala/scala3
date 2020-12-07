import scala.quoted._

case class Position(path: String, start: Int, end: Int,
    startLine: Int, startColumn: Int, endLine: Int, endColumn: Int)

case class Positioned[T](value: T, position: Position)

object Positioned {

  implicit inline def apply[T](x: => T): Positioned[T] = ${impl('x)}

  def impl[T](x: Expr[T])(implicit ev: Type[T], qctx: Quotes): Expr[Positioned[T]] = {
    import quotes.reflect.{Position => Pos, _}
    val pos = Pos.ofMacroExpansion

    val path = Value(pos.sourceFile.jpath.toString)
    val start = Value(pos.start)
    val end = Value(pos.end)
    val startLine = Value(pos.startLine)
    val endLine = Value(pos.endLine)
    val startColumn = Value(pos.startColumn)
    val endColumn = Value(pos.endColumn)

    val liftedPosition = '{new Position($path, $start, $end, $startLine, $startColumn, $endLine, $endColumn)}
    '{Positioned[T]($x, $liftedPosition)}
  }
}
