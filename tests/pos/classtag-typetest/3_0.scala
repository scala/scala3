import scala.language.`3.0`
import scala.reflect.ClassTag

def f3_0[T: ClassTag](x: Any): Unit =
  x match
    case _: T =>
