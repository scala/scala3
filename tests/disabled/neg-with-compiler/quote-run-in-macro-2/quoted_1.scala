import scala.quoted._
import given scala.quoted.autolift._

object Macros {

  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int]) given QuoteContext: Expr[Int] = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
    val y: Int = run(i)
    y
  }
}
