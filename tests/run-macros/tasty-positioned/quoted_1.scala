import scala.quoted._

case class Position(path: String, start: Int, end: Int,
    startLine: Int, startColumn: Int, endLine: Int, endColumn: Int)

case class Positioned[T](value: T, position: Position)

object Positioned {

  implicit inline def apply[T](x: => T): Positioned[T] = ${impl('x)}

  def impl[T](using s: Scope)(x: s.Expr[T])(using s.Type[T]): s.Expr[Positioned[T]] = {
    val pos = s.tasty.rootPosition

    val path = Expr(pos.sourceFile.jpath.toString)
    val start = Expr(pos.start)
    val end = Expr(pos.end)
    val startLine = Expr(pos.startLine)
    val endLine = Expr(pos.endLine)
    val startColumn = Expr(pos.startColumn)
    val endColumn = Expr(pos.endColumn)

    val liftedPosition = '{new Position($path, $start, $end, $startLine, $startColumn, $endLine, $endColumn)}
    '{Positioned[T]($x, $liftedPosition)}
  }
}
