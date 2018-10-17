import scala.quoted._
import scala.quoted.autolift._

object Macros {

  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int]): Staged[Int] = {
    val tb = Toolbox.make
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val y: Int = tb.run(i)
    y
  }
}
