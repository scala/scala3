
import scala.quoted.Expr

object PowerMacro {

  inline def power(inline n: Long, x: Double) = ${powerCode(n, 'x)}

  def powerCode(n: Long, x: Expr[Double]): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
    else '{ $x * ${powerCode(n - 1, x)} }
}
