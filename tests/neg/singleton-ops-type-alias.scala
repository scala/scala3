import scala.compiletime.ops.boolean.*

object Test {
  type Xor[A <: Boolean, B <: Boolean] = (A && ![B]) || (![A] && B)
  val t0: Xor[true, true] = false
  val t1: Xor[false, true] = true
  val t2: Xor[true, false] = false // error
  val t3: Xor[false, false] = true // error
}
