
import dotty.TypeConstraints._

object Test {
  def main(args: Array[String]): Unit = {
    val arr = Array(0)
    foo("", arr) // error
    foo2(' ', arr) // error

    val arr2 = Array(new B)
    foo2(new Object, arr2) // error
    foo2(new A, arr2) // error
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
    val a: X = xs(0) // error
    val b: Y = x
  }

  class A

  class B extends A

}
