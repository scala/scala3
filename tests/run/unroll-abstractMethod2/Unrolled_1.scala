//> using options -experimental
import scala.annotation.unroll

trait Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true): String
}

object UnrolledImpl {
  val impl: Unrolled = new Unrolled {
    def foo(s: String, n: Int, @unroll b: Boolean) = s + n + b
  }
}
