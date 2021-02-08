import scala.compiletime.ops.int.*

object Test {
  type Max2[A <: Int, B <: Int] <: Int = (A < B) match {
    case true => B
    case false => A
  }
  val t0: Max2[-1, 10] = 10
  val t1: Max2[4, 2] = 4
  val t2: Max2[2, 2] = 1 // error
  val t3: Max2[-1, -1] = 0 // error
}
