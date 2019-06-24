
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
      if (n == 0) '{1.0}
      else if (n == 1) x
      else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
      else '{ $x * ${powerCode(n - 1, x)} }

    println(run(powerCode(0, '{5}).show.toExpr))
    println(run(powerCode(1, '{5}).show.toExpr))
    println(run(powerCode(2, '{5}).show.toExpr))
    println(run(powerCode(3, '{5}).show.toExpr))
    println(run(powerCode(22, '{5}).show.toExpr))
  }
}
