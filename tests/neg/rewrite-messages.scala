// scalac: -source:future-migration -deprecation -Werror

import scala.util._  // error

object Test {
  extension (x: Int) def foo(y: Int) = x + y
  2 foo 4  // error
}
