import scala.concurrent.Future
object magnets {

  enum StatusCode {
    case OK, Error
  }

  trait Marshaller[T] { def marshall(t: T): String }
  class HttpResponse

  sealed trait CompletionMagnet {
    type Result
    def apply(): Result
  }

  object CompletionMagnet {

    extend (StatusCode, type T: Marshaller) implements CompletionMagnet {
      type Result = String
      def apply(): String = implicitly[Marshaller[T]].marshall(this._2)
    }

    extend Future[HttpResponse] implements CompletionMagnet {
      type Result = Int
      def apply(): Int = 1
    }

    extend Future[StatusCode] implements CompletionMagnet {
      type Result = Int
      def apply(): Int = 2
    }
  }

  implicit object stringMarshaller extends Marshaller[String] {
    def marshall(x: String): String = x
  }

  def complete(magnet: CompletionMagnet): magnet.Result = magnet()
}

object Test extends App {
  import magnets._

  assert(complete((StatusCode.OK, "hello")) == "hello")

}