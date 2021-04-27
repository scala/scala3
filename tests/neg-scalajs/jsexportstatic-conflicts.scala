import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object
object A {
  @JSExportStatic
  def foo(x: Int): Int = x

  @JSExportStatic("foo")
  def bar(x: Int): Int = x + 1 // error
}

class B extends js.Object
object B {
  @JSExportStatic
  def foo: Int = 1

  @JSExportStatic("foo")
  def bar: Int = 2 // error
}

class C extends js.Object
object C {
  @JSExportStatic
  def foo_=(v: Int): Unit = ()

  @JSExportStatic("foo")
  def bar_=(v: Int): Unit = () // error
}

class D extends js.Object
object D {
  @JSExportStatic
  val a: Int = 1

  @JSExportStatic("a") // error
  var b: Int = 1
}

class E extends js.Object
object E {
  @JSExportStatic
  val a: Int = 1

  @JSExportStatic("a") // error
  def b(x: Int): Int = x + 1
}

class F extends js.Object
object F {
  @JSExportStatic
  def a(x: Int): Int = x + 1

  @JSExportStatic("a") // error
  val b: Int = 1
}

class G extends js.Object
object G {
  @JSExportStatic
  val a: Int = 1

  @JSExportStatic("a") // error
  def b: Int = 2
}

class H extends js.Object
object H {
  @JSExportStatic
  def a: Int = 1

  @JSExportStatic("a") // error
  val b: Int = 2
}

class I extends js.Object
object I {
  @JSExportStatic
  def a: Int = 1

  @JSExportStatic("a") // error
  def b(x: Int): Int = x + 1
}

class J extends js.Object
object J {
  @JSExportStatic
  def a(x: Int): Int = x + 1

  @JSExportStatic("a") // error
  def b: Int = 1
}
