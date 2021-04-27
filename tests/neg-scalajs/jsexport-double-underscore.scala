import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport(name = "__") // error
  def foo: Int = 1

  @JSExport // error
  def bar__(x: Int): Int = x
}

object B {
  @JSExportTopLevel(name = "__") // ok
  val foo: Int = 1

  @JSExportTopLevel("bar__") // ok
  def bar(x: Int): Int = x
}
