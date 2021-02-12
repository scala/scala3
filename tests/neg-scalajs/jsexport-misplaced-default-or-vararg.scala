import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport // error
  def method(x: Int = 1)(y: String): Int = 1
}

class B {
  @JSExport // error
  def method(xs: Int*)(ys: String): Int = 1
}
