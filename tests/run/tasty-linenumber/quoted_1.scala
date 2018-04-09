import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ~lineImpl('[T])(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def lineImpl(x: Type[Unit])(implicit ctx: Context): Expr[LineNumber] =
    '(new LineNumber(~x.toTasty.pos.startLine.toExpr))

}
