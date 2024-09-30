package unroll

import scala.annotation.unroll

object Unrolled{
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true, @unroll l: Long = 0) = s + n + b + l
}
