package unroll

import scala.annotation.unroll

trait Unrolled{
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b
}

object Unrolled extends Unrolled