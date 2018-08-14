import scala.quoted._

object Macros {

  transparent def macro1 = ~ macro1Impl
  def macro1Impl = '(3)

  transparent def macro2(p: Boolean & Constant) = ~ macro2Impl(p)
  def macro2Impl(p: Boolean) = if (p) '(3) else '(4)

  transparent def macro3(n: Int) = ~ macro3Impl('(n))
  def macro3Impl(p: Expr[Int]) = '{ 2 + ~p }

  transparent def macro4(i: Int)(j: Int) = ~ macro4Impl('(i))('(j))
  def macro4Impl(i: Expr[Int])(j: Expr[Int]) = '{ ~i + ~j }

  transparent def macro5(i: Int, j: Int) = ~ macro5Impl(j = '(j), i = '(i))
  def macro5Impl(i: Expr[Int], j: Expr[Int]) = '{ ~i + ~j }

  transparent def power(n: Int & Constant, x: Double) = ~powerCode(n, '(x))

  def powerCode(n: Int, x: Expr[Double]): Expr[Double] =
    if (n == 0) '(1.0)
    else if (n == 1) x
    else if (n % 2 == 0) '{ { val y = ~x * ~x; ~powerCode(n / 2, '(y)) } }
    else '{ ~x * ~powerCode(n - 1, x) }
}
