import scala.scalajs.js
import scala.scalajs.js.annotation.*

// rhs must be = js.native

object Container1 {
  @js.native @JSGlobal("a")
  val a: Int = 1 // error

  @js.native @JSGlobal("b")
  def b: Int = 3 // error

  @js.native @JSGlobal("c")
  def c(x: Int): Int = x + 1 // error
}

// illegal @JSBracketAccess

object Container2 {
  @js.native @JSGlobal("a")
  @JSBracketAccess // error
  val a: Int = js.native

  @js.native @JSGlobal("b")
  @JSBracketAccess // error
  def b: Int = js.native

  @js.native @JSGlobal("c")
  @JSBracketAccess // error
  def c(x: Int): Int = js.native
}

// illegal @JSBracketCall

object Container3 {
  @js.native @JSGlobal("a")
  @JSBracketCall // error
  val a: Int = js.native

  @js.native @JSGlobal("b")
  @JSBracketCall // error
  def b: Int = js.native

  @js.native @JSGlobal("c")
  @JSBracketCall // error
  def c(x: Int): Int = js.native
}

// inferred result type

object Container4 {
  @js.native @JSGlobal("a")
  val a = js.native // error

  @js.native @JSGlobal("b")
  def b = js.native // error
}
