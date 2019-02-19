import scala.quoted._

object Test {

  inline def foo1: Int = { // error
    println()
    ${ impl(1.toExpr) }
  }

  inline def foo2: Int = { // error
    ${ impl(1.toExpr) }
    ${ impl(2.toExpr) }
  }

  inline def foo3: Int = { // error
    val a = 1
    ${ impl('a) }
  }

  inline def foo4: Int = { // error
    ${ impl('{1}) }
    1
  }

  def impl(n: Expr[Int]): Expr[Int] = ???

}
