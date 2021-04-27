import scala.scalajs.js
import scala.scalajs.js.annotation.*

abstract class AbstractParent {
  val a: Int
  def b: Int
  def c(x: Int): Int
}

object Container1 extends AbstractParent {
  @js.native @JSGlobal("a")
  val a: Int = js.native // error

  @js.native @JSGlobal("b")
  def b: Int = js.native // error

  @js.native @JSGlobal("c")
  def c(x: Int): Int = js.native // error
}

class ConcreteParent {
  val a: Int = 1
  def b: Int = 2
  def c(x: Int): Int = x + 1
}

object Container2 extends ConcreteParent {
  @js.native @JSGlobal("a")
  override val a: Int = js.native // error

  @js.native @JSGlobal("b")
  override def b: Int = js.native // error

  @js.native @JSGlobal("c")
  override def c(x: Int): Int = js.native // error
}
