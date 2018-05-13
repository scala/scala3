import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Macros {
  inline def foo(f: => Int => String): String = ~bar('(f))
  def bar(f: Expr[Int => String]): Expr[String] = {
    try {
      val y: Int => String = f.run
      val res = y(3).toExpr // evaluate at compile time
      res.show.toExpr
    } catch {
      case ex: scala.quoted.FreeVariableError =>
        val res = ('((~f)(3))) // evaluate at run time
        res.show.toExpr
    }
  }
}
