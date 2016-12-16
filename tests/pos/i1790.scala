import scala.util.control.NonFatal

class Try[+T] {
  def transform[U](s: T => Try[U], f: Throwable => Try[U]): Try[U] =
    try this match {
      case Success(v) => s(v)
      case Failure(e) => f(e)
    } catch {
      case NonFatal(e) => Failure(e)
    }
}
final case class Success[+T](value: T) extends Try[T]
final case class Failure[+T](exception: Throwable) extends Try[T] {
  def get: T = throw exception
}
