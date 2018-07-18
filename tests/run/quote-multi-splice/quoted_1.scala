
import scala.quoted.Expr

object PowerMacro {

  inline def powerV1(inline n: Long, x: Double) = {
    val y = ~powerCode(Math.max(n, -n), '(x))
    if (n < 0) 1 / y else y
  }

  inline def powerV2(inline n: Long, x: Double) = {
    val a = ~powerCode(n, '(x))
    val b = 1 / ~powerCode(-n, '(x))
    assert(a == b)
    a
  }

  def powerCode(n: Long, x: Expr[Double]): Expr[Double] =
    if (n < 0) '(1.0 / ~powerCode(-n, x))
    else if (n == 0) '(1.0)
    else if (n % 2 == 0) '{ { val y = ~x * ~x; ~powerCode(n / 2, '(y)) } }
    else '{ ~x * ~powerCode(n - 1, x) }
}
