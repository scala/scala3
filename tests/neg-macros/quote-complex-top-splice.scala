import Test.impl

import scala.quoted._

object Test {

  inline def foo1: Unit = ${ // error
    val x = 1
    impl(x)
  }

  inline def foo2: Unit = ${ impl({ // error
    val x = 1
    x
  }) }

  inline def foo3: Unit = ${ impl({ // error
    println("foo3")
    3
  }) }

  inline def foo4: Unit = ${ // error
    println("foo4")
    impl(1)
  }

  def impl(i: Int): Expr[Unit] = '{}

}
