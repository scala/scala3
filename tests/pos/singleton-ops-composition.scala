import scala.compiletime.ops.boolean._
import scala.compiletime.ops.int._

object Test {
  val t0: 1 + 2 * 3 = 7
  val t1: (2 * 7 + 1) % 10 = 5
  val t3: 1 * 1 + 2 * 2 + 3 * 3 + 4 * 4 = 30
  val t4: true && false || true && true || false ^ false = true
}
