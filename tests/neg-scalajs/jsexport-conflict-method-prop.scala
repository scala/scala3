import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A1 {
  @JSExport("a") // error
  def bar(): Int = 2

  @JSExport("a") // error
  val foo = 1
}

class B1 {
  @JSExport("a")
  def bar(): Int = 2
}

class B2 extends B1 {
  @JSExport("a") // error
  def foo_=(x: Int): Unit = ()

  @JSExport("a")
  val foo = 1
}
