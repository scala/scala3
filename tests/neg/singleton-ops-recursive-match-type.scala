import scala.compiletime.ops.int.*

object Test {
  type GCD[A <: Int, B <: Int] <: Int = B match {
    case 0 => A
    case _ => GCD[B, A % B]
  }
  val t0: GCD[10, 0] = 10
  val t1: GCD[252, 105] = 21
  val t3: GCD[105, 147] = 10 // error
  val t4: GCD[1, 1] = -1 // error
}
