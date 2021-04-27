
import scala.quoted.*

inline def power(x: Double, inline n: Int) = ${ powerCode1('x, 'n) }

private def powerCode1(using Quotes)(x: Expr[Double], n: Expr[Int]): Expr[Double] =
  powerCode(x, n.valueOrError)

private def powerCode(using Quotes)(x: Expr[Double], n: Int): Expr[Double] =
  if (n == 0) Expr(1.0)
  else if (n == 1) x
  else if (n % 2 == 0) '{ val y = $x * $x; ${ powerCode('y, n / 2) } }
  else '{ $x * ${ powerCode(x, n - 1) } }
