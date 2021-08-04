package playground

import scala.quoted._

object X {

  inline def power(n: Int, x: Double): Double =
    ${ powerImpl('n, 'x) }

  private def powerImpl(nExpr: Expr[Int], xExpr: Expr[Double])(using Quotes): Expr[Double] =
    nExpr match {
      case Expr(n1) => '{ 42.0 }
      case _ => '{ dynamicPower($nExpr, $xExpr) }
    }

  private def dynamicPower(n: Int, x: Double): Double = {
    println(s"dynamic: $n^$x")
    if (n == 0) 1.0
    else if (n % 2 == 0) dynamicPower(n / 2, x * x)
    else x * dynamicPower(n - 1, x)
  }
}
