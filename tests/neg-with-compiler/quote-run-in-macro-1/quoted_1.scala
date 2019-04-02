import scala.quoted._
import scala.quoted.autolift._

import scala.quoted.Toolbox.Default._

object Macros {
  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int]): Expr[Int] = {
    val y: Int = i.run
    y
  }
}
