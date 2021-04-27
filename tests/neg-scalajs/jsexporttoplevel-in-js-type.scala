import scala.scalajs.js
import scala.scalajs.js.annotation.*

object JSContainer extends js.Object {
  @JSExportTopLevel("A") // error
  object A

  @JSExportTopLevel("B") // error
  object B extends js.Object

  @JSExportTopLevel("C") // error
  class C

  @JSExportTopLevel("D") // error
  class D extends js.Object

  @JSExportTopLevel("e") // error
  val e: Int = 5

  @JSExportTopLevel("f") // error
  var f: Int = 5

  @JSExportTopLevel("g") // error
  def g(x: Int): Int = x + 1
}
