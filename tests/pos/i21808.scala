//> using options -experimental

import scala.util.TupledFunction
import scala.util.NotGiven

object Test {
  type T

  summon[TupledFunction[(x: T, y: T) => x.type, (x: (T, T)) => x._1.type]]
}