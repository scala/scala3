trait TC[X]
object TC {
  given [X: scala.deriving.Mirror.ProductOf]: TC[X] = ???
}

trait Base[T] {
  given TC[T] = scala.compiletime.deferred
}

case class P(x: Int)

object A extends Base[P]
