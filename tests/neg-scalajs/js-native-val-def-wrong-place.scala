import scala.scalajs.js
import scala.scalajs.js.annotation.*

object A {
  val sym = js.Symbol("foo")
}

// In non-native JS object

object NonNativeContainer extends js.Object {
  @js.native // error
  @JSGlobal("a")
  val a: Int = js.native

  @js.native // error
  @JSGlobal("b")
  def b: Int = js.native

  @js.native // error
  @JSGlobal("c")
  def c(x: Int): Int = js.native

  @js.native // error
  @JSName("foo")
  val d: Int = js.native

  @js.native // error
  @JSName("bar")
  def e(x: Int): Int = js.native

  @js.native // error
  @JSName(A.sym)
  val f: Int = js.native

  @js.native // error
  @JSName(A.sym)
  def g(x: Int): Int = js.native
}

// In native JS object, with a native load spec

@js.native @JSGlobal
object NativeContainer extends js.Object {
  @js.native // error
  @JSGlobal("a")
  val a: Int = js.native

  @js.native // error
  @JSGlobal("b")
  def b: Int = js.native

  @js.native // error
  @JSGlobal("c")
  def c(x: Int): Int = js.native

  @js.native // error
  @JSName("foo")
  val d: Int = js.native

  @js.native // error
  @JSName("bar")
  def e(x: Int): Int = js.native

  @js.native // error
  @JSName(A.sym)
  val f: Int = js.native

  @js.native // error
  @JSName(A.sym)
  def g(x: Int): Int = js.native
}

// In native JS object, without native load spec

@js.native @JSGlobal
object NativeContainer2 extends js.Object {
  @js.native // error
  val a: Int = js.native

  @js.native // error
  def b: Int = js.native

  @js.native // error
  def c(x: Int): Int = js.native

  @js.native // error
  val d: Int = js.native

  @js.native // error
  def e(x: Int): Int = js.native

  @js.native // error
  @JSName(A.sym)
  val f: Int = js.native

  @js.native // error
  @JSName(A.sym)
  def g(x: Int): Int = js.native
}

// Local

object ContainerLocal {
  def a(): Unit = {
    @js.native @JSGlobal // error
    val d: Int = js.native

    @js.native @JSGlobal // error
    var e: Int = js.native

    @js.native @JSGlobal // error
    def f: Int = js.native

    @js.native @JSGlobal // error
    def f_=(v: Int): Unit = js.native

    @js.native @JSGlobal // error
    def g(x: Int): Int = js.native

    @js.native @JSGlobal // error
    lazy val h: Int = js.native
  }
}
