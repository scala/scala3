import scala.quoted._

import scala.tasty._
import scala.tasty.util._

object Macros {

  implicit inline def testMacro: Unit = ~impl

  def impl(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

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
