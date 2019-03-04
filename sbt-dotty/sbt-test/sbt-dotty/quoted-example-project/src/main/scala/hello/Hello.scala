package hello

// Import Expr and some extension methods
import scala.quoted._

object Main {

  // Needed to run or show quotes
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

  def main(args: Array[String]): Unit = {
    val square = stagedPower(2)
    println("3^2 = " + square(3))
    println("4.1^2 = " + square(4.1))
    println()
    val cube = stagedPower(3)
    println("2.4^3 = " + cube(2.4))
    println()

    val toTheFourth = stagedPower(4)
    println("3^4 = " + toTheFourth(3))
    println()
  }

  def stagedPower(n: Int): Double => Double = {
    // Code representing the labmda where the recursion is unrolled based on the value of n
    val code = '{ (x: Double) => ${powerCode(n, '{x})}}

    println(s"staged power for n=" + n + ":")
    println(code.show)

    // Evaluate the contents of the code and return it's value
    code.run
  }

  def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n < 0) throw new Exception("Negative powers not implemented. Left as a small exercise. Dont be shy, try it out.")
    else if (n == 2) '{$x * $x}
    else if (n % 2 == 1)  '{$x * ${powerCode(n - 1, x)}}
    else '{ val y = $x * $x; ${powerCode(n / 2, '{y})}}

}