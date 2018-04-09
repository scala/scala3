import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context

case class Position(path: String, start: Int, end: Int,
    startLine: Int, startColumn: Int, endLine: Int, endColumn: Int)

case class Positioned[T](value: T, position: Position)

object Positioned {

  implicit inline def apply[T](x: T): Positioned[T] =
    ~impl('(x))('[T], Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl[T](x: Expr[T])(implicit ev: Type[T], ctx: Context): Expr[Positioned[T]] = {
    val pos = x.toTasty.pos

    val path = pos.sourceFile.toString.toExpr
    val start = pos.start.toExpr
    val end = pos.end.toExpr
    val startLine = pos.startLine.toExpr
    val endLine = pos.endLine.toExpr
    val startColumn = pos.startColumn.toExpr
    val endColumn = pos.endColumn.toExpr

    '(Positioned[T](~x, new Position(~path, ~start, ~end, ~startLine, ~startColumn, ~endLine, ~endColumn)))
  }
}
