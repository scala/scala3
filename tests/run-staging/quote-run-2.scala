
import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope {
    def powerCode(using Scope)(n: Int, x: scope.Expr[Double]): scope.Expr[Double] =
      if (n == 0) '{1.0}
      else if (n == 1) x
      else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
      else '{ $x * ${powerCode(n - 1, x)} }

    println(powerCode(0, '{5.0}).show)
    println(powerCode(1, '{5.0}).show)
    println(powerCode(2, '{5.0}).show)
    println(powerCode(3, '{5.0}).show)
    println(powerCode(22, '{5.0}).show)
  }
}
