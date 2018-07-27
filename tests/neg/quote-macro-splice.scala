import scala.quoted._

object Test {

  transparent def foo1: Int = {
    println()
    ~impl(1.toExpr) // error
  }

  transparent def foo2: Int = {
    ~impl(1.toExpr) // error
    ~impl(2.toExpr) // error
  }

  transparent def foo3: Int = {
    val a = 1
    ~impl('(a))
  }

  transparent def foo4: Int = {
    ~impl('(1))
    1
  }

  def impl(n: Expr[Int]): Expr[Int] = ???

}
