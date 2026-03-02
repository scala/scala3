package b

import a.A
import scala.quoted.*

object B {

  transparent inline def transparentPower(x: Double, inline n: Int): Double =
    ${ powerCode('x, 'n)  }

  def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] = {
    // this macro is invoked during compilation of C.scala. When project a is pipelined
    // This will fail because A.class will never be available, because the classpath entry
    // is the early-output jar. The compiler detects this and aborts macro expansion with an error.

    // see sbt-test/pipelining/pipelining-scala-macro-splice-ok/m/src/main/scala/b/B.scala
    // for a corresponding implementation that uses a class from the same project
    // instead, but succeeds because it can suspend compilation until classes become available.
    def impl(x: Double, n: A): Double =
      if (n.i == 0) 1.0
      else if (n.i % 2 == 1) x * impl(x, A(n.i - 1))
      else impl(x * x, A(n.i / 2))

    Expr(impl(x.valueOrError, A(n.valueOrError)))
  }
}
