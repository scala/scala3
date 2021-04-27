
import scala.compiletime.ops.int.*

object Test {
  type Fib[N <: Int] <: Int = N match {
    case 0 => 0
    case 1 => 1
    case _ => Fib[N - 1] + Fib[N - 2]
  }
  val fib0: Fib[0] = 0
  val fib1: Fib[1] = 1
  val fib2: Fib[2] = 1
  val fib3: Fib[3] = 2
  val fib4: Fib[4] = 3
  val fib5: Fib[5] = 5
  val fib6: Fib[6] = 8
}
