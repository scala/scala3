import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success}

abstract class Lazy[+T](body: => T) extends Future[T] {
  lazy val await = body
  def value = Some(Success(await))
  def isCompleted = true

  def onComplete[U](func: Try[T] => U)(using executor: ExecutionContext) = ???
  def transform[S](f: Try[T] => Try[S])(using executor: ExecutionContext): Future[S] = ???
  def transformWith[S](f: Try[T] => Future[S])(using executor: ExecutionContext): Future[S] = ???
}

class Helper { def foo: Int = 0 }
