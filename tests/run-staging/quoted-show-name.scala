import scala.quoted._
import scala.internal.quoted.showName
import scala.quoted.staging._
import scala.reflect.ClassTag

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println(powerCode(77).show)
  }

  def powerCode(n: Long)(given QuoteContext): Expr[Double => Double] =
    '{ x1 => ${powerCode(n, 2, 'x1)} }

  def powerCode(n: Long, idx: Int, x: Expr[Double])(given QuoteContext): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ @showName(${Expr("x" + idx)}) val y = $x * $x; ${powerCode(n / 2, idx * 2, '{y})} }
    else '{ $x * ${powerCode(n - 1, idx, x)} }

}
