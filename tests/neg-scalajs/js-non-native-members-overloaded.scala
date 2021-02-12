import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A extends js.Object {
  private def foo(i: Int): Int = i // error
  private def foo(s: String): String = s // error
}

object BEnclosing {
  class B extends js.Object {
    private[BEnclosing] def foo(i: Int): Int = i // error
    private def foo(s: String): String = s // error
  }
}

class C extends js.Object {
  private def foo(i: Int): Int = i // error
  def foo(s: String): String = s
}

object DEnclosing {
  class D extends js.Object {
    private[DEnclosing] def foo(i: Int): Int = i // error
    def foo(s: String): String = s
  }
}
