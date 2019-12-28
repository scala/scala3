import Predef.{byte2Byte => _}
import math.Numeric

def consume[T: Numeric](xs: List[T], limit: T): List[T] = xs match
  case x :: xs1 if limit > 0 => consume(xs1, limit - x) // error // error
  case _ => xs

import scala.concurrent.Future

val f = Future[Unit] { } // error

val b: java.lang.Byte = (1: Byte) // error
