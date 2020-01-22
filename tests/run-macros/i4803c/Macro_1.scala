import scala.quoted._

object PowerMacro {
  def powerCode(x: Expr[Double], n: Expr[Long]) with QuoteContext : Expr[Double] =
    powerCode(x, n.value)

  def powerCode(x: Expr[Double], n: Long) with QuoteContext : Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 0) '{ val y = $x * $x; ${powerCode('y, n / 2)} }
    else '{ $x * ${powerCode(x, n - 1)} }
}
