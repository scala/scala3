package concurrent
import scala.util.{Try, Success, Failure}

type Stream[+T] = Future[StreamResult[T]]

enum StreamResult[+T]:
  case More(elem: T, rest: Stream[T])
  case End extends StreamResult[Nothing]

import StreamResult.*

extension [T](c: Channel[Try[T]])
  def toStream(using Async.Config): Stream[T] = Future:
    c.read() match
      case Success(x) => StreamResult.More(x, toStream)
      case Failure(ex: ChannelClosedException) => StreamResult.End
      case Failure(ex) => throw ex

