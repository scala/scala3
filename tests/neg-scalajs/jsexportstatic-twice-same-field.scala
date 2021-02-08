import scala.scalajs.js
import scala.scalajs.js.annotation.*

class StaticContainer extends js.Object

object StaticContainer {
  // Twice as static

  @JSExportStatic // error
  @JSExportStatic("a1")
  val a: Int = 1

  @JSExportStatic // error
  @JSExportStatic("b1")
  var b: Int = 1

  // Once as static and once as top-level

  @JSExportStatic
  @JSExportTopLevel("c1") // error
  val c: Int = 1

  @JSExportStatic
  @JSExportTopLevel("d1") // error
  var d: Int = 1
}
