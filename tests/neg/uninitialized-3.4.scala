//> using options -Werror

import scala.language.`3.4`
import scala.compiletime.uninitialized

class Foo:
  var a: Int = _ // error: migration warning
  var b: Int = uninitialized
