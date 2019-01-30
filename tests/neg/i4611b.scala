import scala.concurrent.Future

class Response
class Request
object Request {
  type To[T] = given Request => T
}

// Don't qualify as SAM type because result type is an implicit function type
trait Responder[T] {
  def responseFor(value: T): Request.To[Future[Response]]
}

object Responder {
  // with SAM
  val responseResponder: Responder[Response] =
    response => Future.successful(response) // error

  // with anonymous class
  val futureResponseResponder: Responder[Future[Response]] = new Responder[Future[Response]] {
    override def responseFor(value: Future[Response]): Request.To[Future[Response]] =
      value
  }
}
