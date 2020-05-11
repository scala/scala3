import scala.quoted._
import scala.quoted.staging._

object Macros {

  given Toolbox = Toolbox.make(getClass.getClassLoader)
  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(using s: Scope)(i: s.Expr[Int]): s.Expr[Int] = {
    val y: Int = run(i) // error
    Expr(y)
  }
}
