import scala.scalajs.js
import scala.scalajs.js.annotation.*

// rhs must be `js.native` (or calling the primary constructor for constructors)

@js.native @JSGlobal
class A1(param: Int) extends js.Object {
  def this(param: Int, z: String) = this(param)
  def this(z: String) = this(z.length, z) // error

  val a: Int = 1 // error
  var b: Int = 2 // error
  def c: Int = 3 // error
  def d(x: Int): Int = x + 1 // error

  @JSBracketAccess
  def e(x: Any): Any = x // error
  @JSBracketCall
  def f(x: Any)(y: String): String = y // error
}

@js.native @JSGlobal
class A2 extends js.Object {
  private[this] def this(x: Int) = this() // ok
  private def this(x: String) = this() // error
  private[A2] def this(x: Boolean) = this() // error

  private[this] val a: Int = js.native // error
  private val b: Int = js.native // error
  private[A2] val c: Int = js.native // error

  private[this] var d: Int = js.native // error
  private var e: Int = js.native // error
  private[A2] var f: Int = js.native // error

  private[this] def g(): Int = js.native // error
  private def h(): Int = js.native // error
  private[A2] def i(): Int = js.native // error
}

object A3 {
  @js.native @JSGlobal("X")
  class X1 private () extends js.Object // error

  @js.native @JSGlobal("X")
  class X2 private[A3] () extends js.Object // error

  @js.native @JSGlobal("X")
  class X3 private[this] () extends js.Object { // ok
    def this(x: Int) = this()
  }
}

@js.native @JSGlobal
class A4 extends js.Object {
  def foo = js.native // error
  val bar = js.native // error
  def assign[T, U](target: T, source: U): T with U = js.native // ok
}

// Members named `apply`

object A5 {
  @js.native
  trait X1 extends js.Object {
    def apply: Int = js.native // error
  }

  @js.native
  trait X2 extends js.Object {
    @JSName("apply")
    def apply: Int = js.native // ok
  }

  @js.native
  trait X3 extends js.Object {
    val apply: Int = js.native // error
  }

  @js.native
  trait X4 extends js.Object {
    @JSName("apply")
    val apply: Int = js.native // ok
  }

  @js.native
  trait X5 extends js.Object {
    var apply: Int = js.native // error
  }

  @js.native
  trait X6 extends js.Object {
    @JSName("apply")
    var apply: Int = js.native // ok
  }
}
