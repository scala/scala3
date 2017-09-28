
import dotty.TypeConstraints._

object Test {
  def main(args: Array[String]): Unit = {
    val arr = Array(0)
    foo(5, arr)
    foo2(6, arr)

    val arr2 = Array(new A)
    foo2(new A, arr2)
    foo2(new B, arr2)
  }

  def foo[X, Y](x: X, xs: Array[Y])(implicit ev: X =::= Y) = {
    xs(0) = ev(x) // deprecated
    xs(0) = x
    val a: X = xs(0)
    val b: Y = x
  }

  def foo2[X, Y](x: X, xs: Array[Y])(implicit ev: X <::< Y) = {
    xs(0) = ev(x) // deprecated
    xs(0) = x
    val b: Y = x
  }

  class A

  class B extends A

}
