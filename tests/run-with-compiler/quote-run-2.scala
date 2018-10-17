
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    def powerCode(n: Int, x: Expr[Double]): Staged[Double] =
      if (n == 0) '(1.0)
      else if (n == 1) x
      else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
      else '{ $x * ${powerCode(n - 1, x)} }

    println(tb.run(powerCode(0, '{5}).show.toExpr))
    println(tb.run(powerCode(1, '{5}).show.toExpr))
    println(tb.run(powerCode(2, '{5}).show.toExpr))
    println(tb.run(powerCode(3, '{5}).show.toExpr))
    println(tb.run(powerCode(22, '{5}).show.toExpr)) // FIXME prin unique names
  }
}
