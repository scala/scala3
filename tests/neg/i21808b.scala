//> using options -experimental

import scala.util.TupledFunction
import scala.util.NotGiven

object Test {
  type T

  summon[TupledFunction[(T, T) => T, (x: (T, T)) => x._1.type]] // error
}