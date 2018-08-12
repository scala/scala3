import scala.quoted._

import scala.quoted.Toolbox.Default._

object Macros {

  rewrite def foreach1(start: Int, end: Int, f: Int => Unit): String = ~impl('(start), '(end), '(f))
  rewrite def foreach2(start: Int, end: Int, f: => Int => Unit): String = ~impl('(start), '(end), '(f))

  def impl(start: Expr[Int], end: Expr[Int], f: Expr[Int => Unit]): Expr[String] = {
    val res = '{
      var i = ~start
      val j = ~end
      while (i < j) {
        ~f.apply('(i))
        i += 1
      }
      do {
        ~f.apply('(i))
        i += 1
      } while (i < j)
    }
    res.show.toExpr
  }
}
