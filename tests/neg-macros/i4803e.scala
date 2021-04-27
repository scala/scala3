import scala.quoted.*

object PowerMacro {
  def power2(x: Expr[Double])(using Quotes) = '{
    inline def power(x: Double, n: Long): Double = // error
      if (n == 0) 1.0
      else if (n % 2 == 0) { val y = x * x; power(y, n / 2) }
      else x * power(x, n - 1)
    power($x, 2)
  }
}
