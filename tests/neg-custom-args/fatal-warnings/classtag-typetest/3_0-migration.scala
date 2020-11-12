import scala.language.`3.0-migration`
import scala.reflect.ClassTag

def f3_0m[T: ClassTag](x: Any): Unit =
  x match
    case _: T =>
