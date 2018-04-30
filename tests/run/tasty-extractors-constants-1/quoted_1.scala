import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Universe
import scala.tasty.util._

object Macros {

  implicit inline def testMacro: Unit =
    ~impl(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl(implicit u: Universe): Expr[Unit] = {
    import u._
    import u.tasty._

    val buff = new StringBuilder
    def stagedPrintln(x: Any): Unit = buff append java.util.Objects.toString(x) append "\n"

    val Constant = new ConstantExtractor(tasty)

    3.toExpr match { case Constant(n) => stagedPrintln(n) }
    '(4) match { case Constant(n) => stagedPrintln(n) }
    '("abc") match { case Constant(n) => stagedPrintln(n) }
    '(null) match { case Constant(n) => stagedPrintln(n) }

    '(new Object) match { case Constant(n) => println(n); case _ => stagedPrintln("OK") }

    '(print(~buff.result().toExpr))
  }
}
