import scala.quoted._
import scala.quoted.staging._
import scala.quoted.autolift.given

object Macros {

  given Toolbox = Toolbox.make(getClass.getClassLoader)
  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int])(given QuoteContext): Expr[Int] = {
    val y: Int = run(i)
    y
  }
}
