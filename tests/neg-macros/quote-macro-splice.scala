import scala.quoted._
import scala.quoted.autolift._

object Test {

  inline def foo1: Int = { // error
    println()
    ${ impl(1) }
  }

  inline def foo2: Int = { // error
    ${ impl(1) }
    ${ impl(2) }
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
