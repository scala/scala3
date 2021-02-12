import Test.impl

import scala.quoted.*

object Test {

  inline def foo1: Unit = ${
    val x = 1 // error
    impl(x) // error
  }

  inline def foo2: Unit = ${ impl(
    { // error
      val x = 1
      x
    }
  ) }

  inline def foo3: Unit = ${ impl(
    { // error
      println("foo3")
      3
    }
  ) }

  inline def foo4: Unit = ${
    println("foo4") // error
    impl(1)
  }

  def impl(i: Int)(using Quotes): Expr[Unit] = '{}

}
