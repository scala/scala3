//> using options -experimental
import scala.annotation.unroll

trait Unrolled {
  final def foo(s: String, n: Int = 1, @unroll b: Boolean = true): String = s + n + b
}

object UnrolledImpl {
  val impl: Unrolled = new Unrolled {}
}
