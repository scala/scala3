import Test.impl

import scala.quoted._

object Test {

  inline def foo1: Unit = ${
    val x = 1 // error
    impl(x)
  }

  inline def foo2: Unit = ${ impl({
    val x = 1 // error
    x
  }) }

  inline def foo3: Unit = ${ impl({
    println("foo3") // error
    3
  }) }

  inline def foo4: Unit = ${
    println("foo4") // error
    impl(1)
  }

  def impl(i: Int) given QuoteContext: Expr[Unit] = '{}

}
