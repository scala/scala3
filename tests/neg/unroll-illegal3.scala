//> using options -experimental

import scala.annotation.unroll

object wrap {
  locally {
    def foo(s: String, @unroll y: Boolean) = s + y // error
  }
}

class UnrolledCls {
  def foo(s: String, @unroll y: Boolean) = s + y // error
}

trait UnrolledTrait {
  def foo(s: String, @unroll y: Boolean): String // error
}
