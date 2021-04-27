import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Names {
  val a = js.Symbol()
}

@js.native
@JSGlobal
class A extends js.Object {
  @JSName(Names.a)
  @JSName("foo") // error
  def a1: Int = js.native

  @JSName("bar")
  @JSName("foo") // error
  def a2: Int = js.native

  @JSName("foo")
  @JSName("foo") // error
  def a3: Int = js.native
}
