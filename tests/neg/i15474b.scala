//> using options -Werror

import scala.language.implicitConversions

object Test1:
  given c: Conversion[ String, Int ]:
    def apply(from: String): Int = from.toInt   // warn: infinite loop in function body
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
