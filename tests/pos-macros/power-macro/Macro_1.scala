
import scala.quoted._

object PowerMacro {

  inline def power(inline n: Long, x: Double) = ${powerCode0('n, 'x)}

  def powerCode0(using s: Scope)(n: s.Expr[Long], x: s.Expr[Double]): s.Expr[Double] =
    powerCode(n.unliftOrError, x)

  def powerCode(using s: Scope)(n: Long, x: s.Expr[Double]): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
    else '{ $x * ${powerCode(n - 1, x)} }
}
