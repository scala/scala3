//> using options -Werror

import scala.language.`future-migration`
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // error: migration warning
  var b: Int = uninitialized
