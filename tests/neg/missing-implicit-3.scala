import Predef.{byte2Byte as _, *}
import math.Numeric

val DAYS = scala.concurrent.duration.DAYS

val d: scala.concurrent.duration.Duration = (10, DAYS) // error

val d2: scala.concurrent.duration.Duration = 10.days // error
