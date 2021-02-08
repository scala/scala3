import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Names {
  val a = "Hello"
  final val b = "World"
  val sym = js.Symbol("foo")
}

class NamesClass {
  val a = js.Symbol("foo")
}

@js.native
@JSGlobal
class B1 extends js.Object {
  @JSName(Names.a) // error
  def foo: Int = js.native
  @JSName(Names.b) // ok
  def bar: Int = js.native
}

class B2 extends js.Object {
  @JSName(Names.a) // error
  def foo: Int = 1
  @JSName(Names.b) // ok
  def bar: Int = 2
}

@js.native
@JSGlobal
class C1 extends js.Object {
  @JSName(js.Symbol()) // error
  def foo: Int = js.native
  @JSName(new NamesClass().a) // error
  def bar: Int = js.native
}

class C2 extends js.Object {
  @JSName(js.Symbol()) // error
  def foo: Int = 1
  @JSName(new NamesClass().a) // error
  def bar: Int = 2
}

object D1 extends js.Object {
  val a = js.Symbol("foo")

  @JSName(a) // warning, untested
  def foo: Int = 1
}

// Implementation restriction
@js.native
@JSGlobal
object E1 extends js.Object {
  @JSName(Names.sym) // error
  @js.native
  class X extends js.Object

  @JSName(Names.sym) // error
  @js.native
  object Y extends js.Object
}
