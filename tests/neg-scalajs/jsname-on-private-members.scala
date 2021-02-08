import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object {
  @JSName("toto1") // error
  private object toto

  @JSName("tata1") // error
  private object tata extends js.Object

  @JSName("Babar1") // error
  private class Babar extends js.Object

  @JSName("a1") // error
  private val a: Int = 0

  @JSName("b1") // error
  private var b: Int = 0

  @JSName("c1") // error
  private def c: Int = 0

  @JSName("d1") // error
  private def d(x: Int): Int = x + 1

  @JSName("e1") // error
  private def e1_=(v: Int): Unit = ()
}
