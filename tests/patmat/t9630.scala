sealed trait OpError
sealed trait RequestErrorType
sealed trait ProcessingErrorType

final case class InvalidEndpoint(reason: String) extends RequestErrorType
final case class InvalidParameters(reason: String) extends RequestErrorType

final case class InvalidFormat(response: String) extends ProcessingErrorType
final case class EntityNotFound(id: Long) extends ProcessingErrorType

final case class RequestError(errorType: RequestErrorType) extends OpError
final case class ProcessingError(errorType: ProcessingErrorType) extends OpError

object Test{
  def printMatches(error: OpError): Unit = error match {
    case RequestError(InvalidEndpoint(reason)) => //print something
    case RequestError(InvalidParameters(reason)) => //print something
    case ProcessingError(InvalidFormat(format)) => //print something
    case ProcessingError(EntityNotFound(entityId)) => //print something
  }
}