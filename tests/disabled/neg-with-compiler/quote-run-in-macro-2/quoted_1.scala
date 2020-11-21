import scala.quoted._

object Macros {

  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int])(using Quotes): Expr[Int] = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    val y: Int = run(i)
    y
  }
}
