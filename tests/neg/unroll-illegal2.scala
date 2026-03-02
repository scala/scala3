//> using options -experimental

import scala.annotation.unroll

class wrap {
  locally {
    final def foo(s: String, @unroll y: Boolean) = s + y // error
  }
}
