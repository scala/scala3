import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait A1 extends js.Object {
  def foo(x: Int): Int
}
trait A2 extends js.Object {
  def bar(x: Int): Int
}
class A3 extends js.Function {
  def foobar(x: Int): Int
}
class A4 {
  val foo: A1 = x => x + 1 // error
  val bar: A2 = x => x + 1 // error
  val foobar: A3 = x => x + 1 // error
}

@js.native
trait B1 extends js.Function {
  def apply(x: Int): Int
}
@js.native
trait B2 extends js.Function {
  def bar(x: Int = 5): Int
}
class B3 {
  val foo: B1 = x => x + 1 // error
  val bar: B2 = x => x + 1 // error
}

trait C1 extends js.Function {
  def foo(x: Int): Int
}
class C2 {
  val foo: C1 = x => x + 1 // error
}
