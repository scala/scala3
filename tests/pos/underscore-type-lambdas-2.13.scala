import scala.language.`2.13`

// `_` is still a wildcard under the Scala 2 compatibility mode used by the standard library
class Test[T]:
  def addAll(xs: Array[_ <: T]): Unit = ()
  def runtimeClass: Class[_] = classOf[Any]
  def stepper[S <: Iterator[_]](s: S): S = s
