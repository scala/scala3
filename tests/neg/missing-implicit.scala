import Predef.{byte2Byte => _, _}
import math.Numeric

def consume[T: Numeric](xs: List[T], limit: T): List[T] = xs match
  case x :: xs1 if limit > 0 => consume(xs1, limit - x) // error // error
  case _ => xs

import scala.concurrent.Future

val f = Future[Unit] { } // error

val b: java.lang.Byte = (1: Byte) // error, but no hint

val DAYS = scala.concurrent.duration.DAYS

val d: scala.concurrent.duration.Duration = (10, DAYS) // error

val d2: scala.concurrent.duration.Duration = 10.days // error
