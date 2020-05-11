import scala.quoted._


object Macros {

  def power_s0(using s: Scope)(x: s.Expr[Double], n: s.Expr[Int]): s.Expr[Double] =
    power_s(x, n.unliftOrError)

  def power_s(using s: Scope)(x: s.Expr[Double], n: Int): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n % 2 == 1) '{ $x * ${power_s(x, n - 1)} }
    else '{ val y = $x * $x; ${power_s('y, n / 2)} }

  inline def power(x: Double, inline n: Int): Double =
    ${power_s0('x, 'n)}

  def power2(x: Double, y: Double): Double = if y == 0.0 then 1.0 else x * power2(x, y - 1.0)

  inline def rewrite(expr: => Double): Double = ${rewrite('expr)}

  // simple, 1-level, non-recursive rewriter for exponents
  def rewrite(using s: Scope)(expr: s.Expr[Double]): s.Expr[Double] = {
    val res = expr match {
      // product rule
      case '{ power2($a, $x) * power2($b, $y)} if a.matches(b) => '{ power2($a, $x + $y) }
      // rules of 1
      case '{ power2($a, 1)} => a
      case '{ power2(1, $a)} => '{ 1.0 }
      // rule of 0
      case '{ power2($a, 0)} => '{ 1.0 }
      // power rule
      case '{ power2(power2($a, $x), $y)} => '{ power2($a, $x * $y ) }
      case _ => expr
    }
    res
  }
}
