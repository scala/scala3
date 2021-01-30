import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  def method(): Unit = {
    @JSExport // error
    class A

    @JSExport // error
    class B extends js.Object

    @JSExport // error
    object C

    @JSExport // error
    object D extends js.Object

    @JSExport // error
    def e = 1

    @JSExport // error
    val f = 1

    @JSExport // error
    var g = 1
  }
}
