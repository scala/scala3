import scala.quoted.*
import scala.quoted.staging.*

object Macros {

  inline def testMacro: Unit = ${impl}

  def impl(using Quotes): Expr[Unit] = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    // 2 is a lifted constant
    val show1 = withQuotes(power('{2}, '{3.0}).show)
    val run1  = run(power('{2}, '{3.0}))

    // n is a lifted constant
    val n = 2
    val show2 = withQuotes(power(Expr(n), '{4.0}).show)
    val run2  = run(power(Expr(n), '{4.0}))

    // n is a constant in a quote
    val show3 = withQuotes(power('{2}, '{5.0}).show)
    val run3  = run(power('{2}, '{5.0}))

    // n2 is not a constant
    def n2(using Quotes) = '{ println("foo"); 2 }
    val show4 = withQuotes(power(n2, '{6.0}).show)
    val run4  = run(power(n2, '{6.0}))

    '{
      println(${Expr(show1)})
      println(${Expr(run1)})
      println()
      println(${Expr(show2)})
      println(${Expr(run2)})
      println()
      println(${Expr(show3)})
      println(${Expr(run3)})
      println()
      println(${Expr(show4)})
      println(${Expr(run4)})
    }
  }

  def power(n: Expr[Int], x: Expr[Double])(using Quotes): Expr[Double] = {
    n.value match {
      case Some(n1) => powerCode(n1, x)
      case _ => '{ dynamicPower($n, $x) }
    }
  }

  def powerCode(n: Int, x: Expr[Double])(using Quotes): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
    else '{ $x * ${powerCode(n - 1, x)} }

  def dynamicPower(n: Int, x: Double): Double =
    if (n == 0) 1.0
    else if (n % 2 == 0) dynamicPower(n / 2, x * x)
    else x * dynamicPower(n - 1, x)
}
