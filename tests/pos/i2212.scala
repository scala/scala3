package object squants {
  type Time = squants.time.Time
}
package squants.time {
  class Time
  object Time { def x = 2 }
}
package squants.velocity {
  import squants.time._  // <-- imports `Time` value
  import squants.Time // <-- imports  type alias
  object Velocity { Time.x }
}

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

object Money {
  def foo(round: RoundingMode = RoundingMode.HALF_EVEN): Int = ???
}
