import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object {
  def a: Unit = ()
  @JSName("a")
  def b: Unit = () // error

  class B1 extends js.Object
  object B1 // error
}
