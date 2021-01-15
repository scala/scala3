import scala.quoted._
import scala.quoted.staging._

object Macros {

  given Compiler = Compiler.make(getClass.getClassLoader)
  inline def foo(i: => Int): Int = ${ fooImpl('i) }
  def fooImpl(i: Expr[Int])(using Quotes): Expr[Int] = {
    val y: Int = run(i)
    Expr(y)
  }
}
