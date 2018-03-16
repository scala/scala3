import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Test {

  def main(args: Array[String]): Unit = {
    // 2 is a lifted constant
    println(power(2.toExpr, 3.0.toExpr).show)
    println(power(2.toExpr, 3.0.toExpr).run)
  }

  def power(n: Expr[Int], x: Expr[Double]): Expr[Double] = {
    n match {
      case Constant(n1) => powerCode(n1, x)
      case _ => '{ dynamicPower(~n, ~x) }
    }
  }

  private def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
    if (n == 0) '(1.0)
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = ~x * ~x; ~powerCode(n / 2, '(y)) } }
    else '{ ~x * ~powerCode(n - 1, x) }

  def dynamicPower(n: Int, x: Double): Double =
    if (n == 0) 1.0
    else if (n % 2 == 0) dynamicPower(n / 2, x * x)
    else x * dynamicPower(n - 1, x)
}
