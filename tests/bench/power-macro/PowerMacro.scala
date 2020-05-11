import scala.quoted._

object PowerMacro {

  inline def power(inline n: Long, x: Double) = ${ powerCode('n, 'x) }

  def powerCode(using s: Scope)(n: s.Expr[Long], x: s.Expr[Double]): s.Expr[Double] =
    powerCodeUnrolled(n.unliftOrError, x)

  def powerCodeUnrolled(using s: Scope)(n: Long, x: s.Expr[Double]): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 0) '{ val y = $x * $x; ${powerCodeUnrolled(n / 2, 'y)} }
    else '{ $x * ${powerCodeUnrolled(n - 1, x)} }

}