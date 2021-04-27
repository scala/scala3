import Predef.{byte2Byte as _, *}
import math.Numeric

def consume[T: Numeric](xs: List[T], limit: T): List[T] = xs match
  case x :: xs1 if limit > 0 => consume(xs1, limit - x) // error // error
  case _ => xs
