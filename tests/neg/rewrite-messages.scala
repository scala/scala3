//> using options -source:future-migration -deprecation -Werror

import scala.util._  // warn

object Test {
  extension (x: Int) def foo(y: Int) = x + y
  2 foo 4  // warn
}
// nopos-error: No warnings can be incurred under -Werror.