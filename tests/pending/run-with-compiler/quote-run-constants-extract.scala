import scala.quoted._

import dotty.tools.dotc.quoted.Toolbox._

object Test {

  def main(args: Array[String]): Unit = {
    (3: Expr[Int]) match { case Constant(n) => println(n) }
    '(4) match { case Constant(n) => println(n) }
    '("abc") match { case Constant(n) => println(n) }
    '(null) match { case Constant(n) => println(n) }

    '(new Object) match { case Constant(n) => println(n); case _ => println("OK") }


    // 2 is a lifted constant
    println(power(2, 3.0).show)
    println(power(2, 3.0).run)

    // n is a lifted constant
    val n = 2
    println(power(n, 4.0).show)
    println(power(n, 4.0).run)

    // n is a constant in a quote
    println(power('(2), 5.0).show)
    println(power('(2), 5.0).run)

    // n2 is clearly not a constant
    val n2 = '{ println("foo"); 2 }
    println(power(n2, 6.0).show)
    println(power(n2, 6.0).run)
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
