
trait TC[T]

trait Base[T] {
  given TC[T] = scala.compiletime.deferred
}

object A extends Base[A.P] { // error
  case class P()
}
