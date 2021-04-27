import scala.quoted.*

object Macros {

  inline def macro1 = ${ macro1Impl }
  def macro1Impl (using Quotes)= '{3}

  inline def macro2(inline p: Boolean) = ${ macro2Impl('p) }
  def macro2Impl(p: Expr[Boolean]) (using Quotes)= if (p.valueOrError) '{3} else '{4}

  inline def macro3(n: Int) = ${ macro3Impl('n) }
  def macro3Impl(p: Expr[Int]) (using Quotes)= '{ 2 + $p }

  inline def macro4(i: Int)(j: Int) = ${ macro4Impl('i)('j) }
  def macro4Impl(i: Expr[Int])(j: Expr[Int]) (using Quotes)= '{ $i + $j }

  inline def macro5(i: Int, j: Int) = ${ macro5Impl(j = 'j, i = 'i) }
  def macro5Impl(i: Expr[Int], j: Expr[Int]) (using Quotes)= '{ $i + $j }

  inline def power(inline n: Int, x: Double) = ${ powerCode('n, 'x) }

  def powerCode(n: Expr[Int], x: Expr[Double]) (using Quotes): Expr[Double] =
    powerCode(n.valueOrError, x)

  def powerCode(n: Int, x: Expr[Double]) (using Quotes): Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}
