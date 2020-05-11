import scala.quoted._
import scala.quoted.staging.{run, usingNewScope, Toolbox}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    val toTheEighth = stagedPower(8)
    println("3^8 = " + toTheEighth(3))

    val toThe47 = stagedPower(47)
    println("2^47 = " + toThe47(2))
  }

  def stagedPower(n: Int): Double => Double = {
    def code(using Scope) = '{ (x: Double) => ${ powerCode(n, 'x) } }
    println("The following would not compile:")
    println(usingNewScope(code.show))
    run(code)
  }

  def powerCode(using s: Scope)(n: Int, x: s.Expr[Double]): s.Expr[Double] =
    if (n == 1) x
    else if (n == 2) '{ $x * $x }
    else if (n % 2 == 1)  '{ $x * ${ powerCode(n - 1, x) } }
    else '{ val y = $x * $x; ${ powerCode(n / 2, 'y) } }
}
