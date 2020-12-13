import scala.language.`3.1`
import scala.reflect.ClassTag

def f3_1[T: ClassTag](x: Any): Unit =
  x match
    case _: T =>
