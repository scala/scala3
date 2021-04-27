import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {
  @JSExportTopLevel("A")
  val a: Int = 1

  @JSExportTopLevel("A") // error
  var b: Int = 1
}

object B {
  @JSExportTopLevel("B")
  val a: Int = 1

  @JSExportTopLevel("B") // error
  def b(x: Int): Int = x + 1
}

object C {
  @JSExportTopLevel("C")
  def a(x: Int): Int = x + 1

  @JSExportTopLevel("C") // error
  val b: Int = 1
}
