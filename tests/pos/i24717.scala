import scala.compiletime.ops.int.+
import scala.compiletime.ops.int.S

object test {
  object O {
    opaque type O = Int
    transparent inline def v: O = 123
  }

  val a: 123 & O.O = O.v
  val b: S[a.type] = 124
  val c: a.type + 1 = 124
}
