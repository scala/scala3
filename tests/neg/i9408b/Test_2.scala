//> using options -Xfatal-warnings

import language.`3.0-migration`
import scala.language.implicitConversions

object Test {
  import test.conversions.Conv.*
  val length: Int = "abc" // warn
}

// nopos-error: No warnings can be incurred under -Werror.