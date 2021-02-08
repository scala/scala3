import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Container {
  // setters

  @js.native @JSGlobal("foo") // error
  def foo_=(x: Int): Int = js.native
  @js.native @JSGlobal("bar") // error
  def bar_=(x: Int, y: Int): Unit = js.native
  @js.native @JSGlobal("goo") // error
  def goo_=(x: Int*): Unit = js.native
  @js.native @JSGlobal("hoo") // error
  def hoo_=(x: Int = 1): Unit = js.native

  // vars

  @js.native @JSGlobal("foo") // error
  var x: Int = js.native

  // lazy vals

  @js.native @JSGlobal("foo") // error
  lazy val y: Int = js.native
}
