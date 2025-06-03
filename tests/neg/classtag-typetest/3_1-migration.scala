//> using options -Xfatal-warnings

import scala.language.`future-migration`
import scala.reflect.ClassTag

def f3_1m[T: ClassTag](x: Any): Unit =
  x match
    case _: T => // warn
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
