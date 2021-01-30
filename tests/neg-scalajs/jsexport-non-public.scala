import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Container {

  // Classes

  @JSExport // error
  private class A1

  @JSExport // error
  protected[this] class A2

  @JSExport // error
  private[Container] class A3

  @JSExport // error
  private class A4 extends js.Object

  @JSExport // error
  protected[this] class A5 extends js.Object

  @JSExport // error
  private[Container] class A6 extends js.Object

  // Objects

  @JSExport // error
  private object B1

  @JSExport // error
  protected[this] object B2

  @JSExport // error
  private[Container] object B3

  @JSExport // error
  private object B4 extends js.Object

  @JSExport // error
  protected[this] object B5 extends js.Object

  @JSExport // error
  private[Container] object B6 extends js.Object

  // Vals and defs

  @JSExport // error
  private val c1: Int = 5

  @JSExport // error
  protected[this] var c2: Int = 5

  @JSExport // error
  private[Container] def x(x: Int): Int = 5

}
