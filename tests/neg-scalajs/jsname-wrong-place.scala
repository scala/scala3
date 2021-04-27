import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Sym {
  val sym = js.Symbol()
}

// On non-native JS types

@JSName("foo") // error
class A1 extends js.Object

@JSName("foo") // error
trait A2 extends js.Object

@JSName("foo") // error
object A3 extends js.Object

@JSName(Sym.sym) // error
class A4 extends js.Object

@JSName(Sym.sym) // error
trait A5 extends js.Object

@JSName(Sym.sym) // error
object A6 extends js.Object

// On Scala types

@JSName("foo") // error
class B1

@JSName("foo") // error
trait B2

@JSName("foo") // error
object B3

@JSName(Sym.sym) // error
class B4

@JSName(Sym.sym) // error
trait B5

@JSName(Sym.sym) // error
object B6

// On vals and defs

object Container1 {
  @JSName("foo") // error
  val a: Int = 1

  @JSName("foo") // error
  var b: Int = 2

  @JSName("foo") // error
  def c: Int = 3

  @JSName("foo") // error
  def d_=(v: Int): Unit = ()

  @JSName("foo") // error
  def e(x: Int): Int = x + 1
}

// On native JS things

object Container2 {
  @js.native @JSGlobal("X")
  @JSName("X") // error
  class X extends js.Object

  @js.native @JSGlobal("Y")
  @JSName("Y") // error
  object Y extends js.Object

  @js.native @JSGlobal("a")
  @JSName("a") // error
  val a: Int = js.native

  @js.native @JSGlobal("b") // error
  @JSName("b") // error
  var b: Int = js.native

  @js.native @JSGlobal("c")
  @JSName("c") // error
  def c: Int = js.native

  @js.native @JSGlobal("d") // error
  @JSName("d") // error
  def d_=(v: Int): Unit = js.native

  @js.native @JSGlobal("e")
  @JSName("e") // error
  def e(x: Int): Int = js.native
}
