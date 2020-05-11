import scala.quoted._

object PowerMacro {
  def powerCode0(using s: Scope)(x: s.Expr[Double], n: s.Expr[Long]): s.Expr[Double] =
    powerCode(x, n.unliftOrError)

  def powerCode(using s: Scope)(x: s.Expr[Double], n: Long): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode('y, n / 2) } }
    else '{ $x * ${ powerCode(x, n - 1) } }
}
