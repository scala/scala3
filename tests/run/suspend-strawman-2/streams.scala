package concurrent
import scala.util.{Try, Success, Failure}

type Stream[+T] = Future[StreamResult[T]]

enum StreamResult[+T]:
  case More(elem: T, rest: Stream[T])
  case End extends StreamResult[Nothing]

import StreamResult.*

extension [T](c: Channel[T])
  def toStream(using Async.Config): Stream[T] = Future:
    try StreamResult.More(c.read(), toStream)
    catch case ex: ChannelClosedException => StreamResult.End
