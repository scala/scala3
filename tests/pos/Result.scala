import scala.util.control.NonFatal
object p {

  enum Result[+T, +E] {
    case OK [T](x: T) extends Result[T, Nothing]
    case Err[E](e: E) extends Result[Nothing, E]
  }

  type Try[T] = Result[T, Throwable]
  object Try {
    def apply[T](x: => T): Try[T] =
      try Result.OK(x)
      catch {
        case NonFatal(ex) => Result.Err(ex)
      }
  }
}
