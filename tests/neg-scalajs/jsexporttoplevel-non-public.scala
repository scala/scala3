import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Container {

  // Classes

  @JSExportTopLevel("A1") // error
  private class A1

  @JSExportTopLevel("A2") // error
  protected[this] class A2

  @JSExportTopLevel("A3") // error
  private[Container] class A3

  @JSExportTopLevel("A4") // error
  private class A4 extends js.Object

  @JSExportTopLevel("A5") // error
  protected[this] class A5 extends js.Object

  @JSExportTopLevel("A6") // error
  private[Container] class A6 extends js.Object

  // Objects

  @JSExportTopLevel("B1") // error
  private object B1

  @JSExportTopLevel("B2") // error
  protected[this] object B2

  @JSExportTopLevel("B3") // error
  private[Container] object B3

  @JSExportTopLevel("B4") // error
  private object B4 extends js.Object

  @JSExportTopLevel("B5") // error
  protected[this] object B5 extends js.Object

  @JSExportTopLevel("B6") // error
  private[Container] object B6 extends js.Object

  // Vals and defs

  @JSExportTopLevel("c1") // error
  private val c1: Int = 5

  @JSExportTopLevel("c2") // error
  protected[this] var c2: Int = 5

  @JSExportTopLevel("c3") // error
  private[Container] def x(x: Int): Int = 5

}
