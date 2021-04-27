import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {
  @JSExportTopLevel("a1") // error
  lazy val a1: Int = 1

  @JSExportTopLevel("a2") // error
  def a2: Int = 1

  @JSExportTopLevel("a3") // error
  def a3_=(v: Int): Unit = ()
}
