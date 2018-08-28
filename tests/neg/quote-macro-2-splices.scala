import scala.quoted._

object Macro {

  rewrite def foo(b: Boolean): Int = { // error
    if (b) ~bar(true)
    else ~bar(false)
  }

  def bar(b: Boolean): Expr[Int] = if (b) '(1) else '(0)
}
