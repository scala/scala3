import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

case class Position(path: String, start: Int, end: Int,
    startLine: Int, startColumn: Int, endLine: Int, endColumn: Int)

case class Positioned[T](value: T, position: Position)

object Positioned {

  implicit inline def apply[T](x: => T): Positioned[T] = ${impl('x)}

  def impl[T](x: Expr[T])(implicit ev: Type[T], reflect: Reflection): Expr[Positioned[T]] = {
    import reflect.{Position => _, _}
    val pos = rootPosition

    val path = pos.sourceFile.jpath.toString
    val start = pos.start
    val end = pos.end
    val startLine = pos.startLine
    val endLine = pos.endLine
    val startColumn = pos.startColumn
    val endColumn = pos.endColumn

    val liftedPosition = '{new Position($path, $start, $end, $startLine, $startColumn, $endLine, $endColumn)}
    '{Positioned[T]($x, $liftedPosition)}
  }
}
