import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe

class LineNumber(val value: Int) {
  override def toString: String = value.toString
}

object LineNumber {

  implicit inline def line[T >: Unit <: Unit]: LineNumber =
    ~lineImpl('[T])(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def lineImpl(x: Type[Unit])(implicit u: Universe): Expr[LineNumber] = {
    import u._
    import u.tasty._
    '(new LineNumber(~x.toTasty.pos.startLine.toExpr))
  }

}
