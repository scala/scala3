import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  class A1 extends js.Object

  object A1 {
    @JSExportStatic // error
    def a(): Unit = ()
  }
}

class B extends js.Object

object B extends js.Object {
  @JSExportStatic // error
  def a(): Unit = ()
}

class C extends js.Object

@js.native
@JSGlobal("Dummy")
object C extends js.Object {
  @JSExportStatic // error
  def a(): Unit = js.native
}

class D

object D {
  @JSExportStatic // error
  def a(): Unit = ()
}

trait E extends js.Object

object E {
  @JSExportStatic // error
  def a(): Unit = ()
}

@js.native
@JSGlobal("Dummy")
class F extends js.Object

object F {
  @JSExportStatic // error
  def a(): Unit = ()
}
