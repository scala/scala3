import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object {
  def apply(arg: Int): Int = arg // error

  @JSBracketAccess
  def foo(index: Int, arg: Int): Int = arg // error

  @JSBracketCall
  def foo(m: String, arg: Int): Int = arg // error
}

class B extends js.Object {
  def apply: Int = 42 // error
}

class BOK extends js.Object {
  @JSName("apply")
  def apply: Int = 42
}

class C extends js.Object {
  val apply: Int = 42 // error
}

class COK extends js.Object {
  @JSName("apply")
  val apply: Int = 42
}

class D extends js.Object {
  var apply: Int = 42 // error
}

class DOK extends js.Object {
  @JSName("apply")
  var apply: Int = 42
}
