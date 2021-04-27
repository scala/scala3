import scala.quoted.*
import scala.quoted.staging.{run, withQuotes, Compiler}

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    val toTheEighth = stagedPower(8)
    println("3^8 = " + toTheEighth(3))

    val toThe47 = stagedPower(47)
    println("2^47 = " + toThe47(2))
  }

  def stagedPower(n: Int): Double => Double = {
    def code(using Quotes) = '{ (x: Double) => ${ powerCode(n, 'x) } }
    println("The following would not compile:")
    println(withQuotes(code.show))
    run(code)
  }

  def powerCode(n: Int, x: Expr[Double])(using ctx: Quotes): Expr[Double] =
    if (n == 1) x
    else if (n == 2) '{ $x * $x }
    else if (n % 2 == 1)  '{ $x * ${ powerCode(n - 1, x) } }
    else '{ val y = $x * $x; ${ powerCode(n / 2, 'y) } }
}
