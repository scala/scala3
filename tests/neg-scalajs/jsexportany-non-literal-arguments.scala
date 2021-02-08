import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Names {
  val a = "Hello"
  final val b = "World"
}

class A {
  @JSExport(Names.a) // error
  def foo: Int = 1
  @JSExport(Names.b) // ok
  def bar: Int = 1
}

object B {
  @JSExportTopLevel("foo", Names.a) // error
  def foo(): Int = 1
  @JSExportTopLevel("bar", Names.b) // ok
  def bar(): Int = 1
}

object C {
  @JSExportTopLevel(Names.a, "foo") // error
  def foo(): Int = 1
  @JSExportTopLevel(Names.b, "bar") // ok
  def bar(): Int = 1
}

class D extends js.Object
object D {
  @JSExportStatic(Names.a) // error
  def foo(): Int = 1
  @JSExportStatic(Names.b) // ok
  def bar(): Int = 1
}
