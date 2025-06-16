//> using options -experimental

import scala.annotation.unroll

class Unrolled {
  final def foo(@unroll x: Int = 0)[T](// error
    s: T,
    @unroll y: Boolean = true,
  ): String = "" + x + s + y
}
