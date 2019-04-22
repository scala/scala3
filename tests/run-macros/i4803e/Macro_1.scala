import scala.quoted._

object PowerMacro {
  def power2(x: Expr[Double]) = '{
    inline def power(x: Double, n: Long): Double =
      if (n == 0) 1.0
      else if (n % 2 == 0) { val y = x * x; power(y, n / 2) }
      else x * power(x, n - 1)
    power($x, 2)
  }
}
