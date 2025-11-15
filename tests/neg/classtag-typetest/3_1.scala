//> using options -Werror

import scala.language.future
import scala.reflect.ClassTag

def f3_1[T: ClassTag](x: Any): Unit =
  x match
    case _: T => // warn
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
