import scala.scalajs.js
import scala.scalajs.js.annotation.*

// Non-native JS types

@JSGlobal // error
class A1 extends js.Object

@JSGlobal // error
trait A2 extends js.Object

@JSGlobal // error
object A3 extends js.Object

@JSGlobal("Foo") // error
class B1 extends js.Object

@JSGlobal("Foo") // error
object B2 extends js.Object

@JSGlobal("Foo") // error
object B3 extends js.Object

// Scala types

@JSGlobal // error
class C1

@JSGlobal // error
trait C2

@JSGlobal // error
object C3

@JSGlobal("Foo") // error
class D1

@JSGlobal("Foo") // error
object D2

@JSGlobal("Foo") // error
object D3

// Fields and methods

object Container {
  @JSGlobal // error
  val a: Int = 1

  @JSGlobal // error
  var b: Int = 2

  @JSGlobal // error
  def c: Int = 3

  @JSGlobal // error
  def d_=(v: Int): Unit = ()

  @JSGlobal // error
  def e(x: Int): Int = x + 1
}

// @JSImport

@JSImport("foo.js", "E1") // error
class E1 extends js.Object

@JSImport("foo.js", "E2", globalFallback = "E") // error
class E2 extends js.Object

// @JSGlobalScope

@JSGlobalScope // error
object F1 extends js.Object

@JSGlobalScope // error
object F2
