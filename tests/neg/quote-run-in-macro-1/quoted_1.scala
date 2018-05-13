import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Macros {
  inline def foo(i: => Int): Int = ~{
    val y: Int = ('(i)).run
    y.toExpr
  }
}
