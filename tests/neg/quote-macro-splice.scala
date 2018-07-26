import scala.quoted._

object Test {

  transparent def foo1: Int = { // error
    println()
    ~impl(1.toExpr)
  }

  transparent def foo2: Int = { // error
    ~impl(1.toExpr)
    ~impl(2.toExpr)
  }

  transparent def foo3: Int = { // error
    val a = 1
    ~impl('(a))
  }

  transparent def foo4: Int = { // error
    ~impl('(1))
    1
  }

  def impl(n: Expr[Int]): Expr[Int] = ???

}
