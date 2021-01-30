import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport("toString") // error
  def a(): Int = 5
}

class B {
  @JSExport("toString") // ok
  def a(x: Int): Int = x + 1
}
