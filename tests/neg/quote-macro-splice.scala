import scala.quoted._

object Test {

  transparent def foo1: Int = {
    println()
    ~impl(1.toExpr) // error: splice outside quotes
  }

  transparent def foo2: Int = {
    ~impl(1.toExpr)  // error: splice outside quotes
    ~impl(2.toExpr)  // error: splice outside quotes
  }

  transparent def foo3: Int = {
    val a = 1
    ~impl('(a)) // error: splice outside quotes
  }

  transparent def foo4: Int = {
    ~impl('(1))  // error: splice outside quotes
    1
  }

  def impl(n: Expr[Int]): Expr[Int] = ???

}
