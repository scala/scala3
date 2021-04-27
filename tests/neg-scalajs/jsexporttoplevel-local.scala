import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  def method(): Unit = {
    @JSExportTopLevel("A") // error
    class A

    @JSExportTopLevel("B") // error
    class B extends js.Object

    @JSExportTopLevel("C") // error
    object C

    @JSExportTopLevel("D") // error
    object D extends js.Object

    @JSExportTopLevel("e") // error
    def e(): Int = 1

    @JSExportTopLevel("f") // error
    val f = 1

    @JSExportTopLevel("g") // error
    var g = 1
  }
}
