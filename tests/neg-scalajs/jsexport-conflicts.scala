import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport
  def rtType(x: js.Any): js.Any = x

  @JSExport // error
  def rtType(x: js.Dynamic): js.Dynamic = x
}

class B {
  @JSExport
  def foo(x: Int)(ys: Int*): Int = x

  @JSExport // error
  def foo(x: Int*): Seq[Int] = x
}

class C {
  @JSExport
  def foo(x: Int = 1): Int = x
  @JSExport // error
  def foo(x: String*): Seq[String] = x
}

class D {
  @JSExport
  def foo(x: Double, y: String)(z: Int = 1): Double = x
  @JSExport // error
  def foo(x: Double, y: String)(z: String*): Double = x
}

class E {
  @JSExport
  def a(x: scala.scalajs.js.Any): Int = 1

  @JSExport // error
  def a(x: Any): Int = 2
}
