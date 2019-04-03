import scala.quoted._
import scala.quoted.autolift._

object Macros {

  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int]): Expr[Int] = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val y: Int = i.run
    y
  }
}
