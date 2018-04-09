import scala.quoted._
import dotty.tools.dotc.quoted.Toolbox._

import scala.tasty.Context
import scala.tasty.names.Name
import scala.tasty.trees._
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def testMacro: Unit =
    ~impl(Context.compilationContext) // FIXME infer Context.compilationContext within top level ~

  def impl(implicit ctx: Context): Expr[Unit] = {
    val buff = new StringBuilder
    def stagedPrintln(x: Any): Unit = buff append java.util.Objects.toString(x) append "\n"

    3.toExpr match { case Constant(n) => stagedPrintln(n) }
    '(4) match { case Constant(n) => stagedPrintln(n) }
    '("abc") match { case Constant(n) => stagedPrintln(n) }
    '(null) match { case Constant(n) => stagedPrintln(n) }

    '(new Object) match { case Constant(n) => println(n); case _ => stagedPrintln("OK") }

    '(print(~buff.result().toExpr))
  }
}
