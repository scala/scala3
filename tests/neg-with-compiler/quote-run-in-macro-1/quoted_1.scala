import scala.quoted._

import scala.quoted.Toolbox.Default._

object Macros {
  transparent def foo(i: => Int): Int = ~{
    val y: Int = ('(i)).run
    y.toExpr
  }
}
