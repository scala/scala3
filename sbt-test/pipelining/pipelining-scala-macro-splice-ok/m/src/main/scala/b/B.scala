package b

import a.A
import scala.quoted.*

object B {

  transparent inline def transparentPower(x: Double, inline n: Int): Double =
    ${ powerCode('x, 'n)  }

  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] = {
    // this macro will cause a suspension in compilation of C.scala, because it calls
    // transparentPower. This will try to invoke the macro but fail because A.class
    // is not yet available until the run for A.scala completes.

    // see sbt-test/pipelining/pipelining-scala-macro-splice/m/src/main/scala/b/B.scala
    // for a corresponding implementation that uses a class from an upstream project
    // instead, and fails because pipelining is turned on for the upstream project.
    def impl(x: Double, n: A): Double =
      if (n.i == 0) 1.0
      else if (n.i % 2 == 1) x * impl(x, A(n.i - 1))
      else impl(x * x, A(n.i / 2))

    Expr(impl(x.valueOrError, A(n.valueOrError)))
  }
}
