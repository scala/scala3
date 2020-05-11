import scala.quoted._

object Macros {

  inline def macro1 = ${ macro1Impl }
  def macro1Impl(using s: Scope) = '{3}

  inline def macro2(inline p: Boolean) = ${ macro2Impl('p) }
  def macro2Impl(using s: Scope)(p: s.Expr[Boolean]) = if (p.unliftOrError) '{3} else '{4}

  inline def macro3(n: Int) = ${ macro3Impl('n) }
  def macro3Impl(using s: Scope)(p: s.Expr[Int]) = '{ 2 + $p }

  inline def macro4(i: Int)(j: Int) = ${ macro4Impl('i)('j) }
  def macro4Impl(using s: Scope)(i: s.Expr[Int])(j: s.Expr[Int])= '{ $i + $j }

  inline def macro5(i: Int, j: Int) = ${ macro5Impl(j = 'j, i = 'i) }
  def macro5Impl(using s: Scope)(i: s.Expr[Int], j: s.Expr[Int])= '{ $i + $j }

  inline def power(inline n: Int, x: Double) = ${ powerCode0('n, 'x) }

  def powerCode0(using s: Scope)(n: s.Expr[Int], x: s.Expr[Double]): s.Expr[Double] =
    powerCode(n.unliftOrError, x)

  def powerCode(using s: Scope)(n: Int, x: s.Expr[Double]): s.Expr[Double] =
    if (n == 0) '{1.0}
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = $x * $x; ${powerCode(n / 2, 'y)} } }
    else '{ $x * ${ powerCode(n - 1, x) } }
}
