import Test.impl

import scala.quoted._

object Test {

  transparent def foo1: Unit = ~{ // error
    val x = 1
    impl(x)
  }

  transparent def foo2: Unit = ~impl({ // error
    val x = 1
    x
  })

  transparent def foo3: Unit = ~impl({ // error
    println("foo3")
    3
  })

  transparent def foo4: Unit = ~{ // error
    println("foo4")
    impl(1)
  }

  def impl(i: Int): Expr[Unit] = '()

}
