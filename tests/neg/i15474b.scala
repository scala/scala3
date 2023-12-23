//> using options -Xfatal-warnings

import scala.language.implicitConversions

object Test1:
  given c: Conversion[ String, Int ] with
    def apply(from: String): Int = from.toInt   // error: infinite loop in function body

