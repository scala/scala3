import scala.quoted._
import scala.quoted.util.let
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println(powerCode(77).show)
  }

  def powerCode(n: Long)(given QuoteContext): Expr[Double => Double] =
    '{ x => ${powerCode(n, 2, 'x)} }

  def powerCode(n: Long, idx: Int, x: Expr[Double])(given QuoteContext): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) let('{ $x * $x })(y => powerCode(n / 2, idx * 2, y))
    else '{ $x * ${powerCode(n - 1, idx, x)} }

}
