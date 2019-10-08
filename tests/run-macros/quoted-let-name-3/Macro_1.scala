import scala.quoted._
import scala.quoted.util.let

inline def power(x: Double, inline n: Long): String =
  ${ powerCodeShow('x, n) }

private def powerCodeShow(x: Expr[Double], n: Long)(given QuoteContext): Expr[String] =
  Expr(powerCode(n, 2, x).show)

private def powerCode(n: Long, idx: Int, x: Expr[Double])(given QuoteContext): Expr[Double] =
  if (n == 0) '{1.0}
  else if (n == 1) x
  else if (n % 2 == 0) let('{ $x * $x })(y => powerCode(n / 2, idx * 2, y))
  else '{ $x * ${powerCode(n - 1, idx, x)} }
