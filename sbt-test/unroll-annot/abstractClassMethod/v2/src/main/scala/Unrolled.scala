package unroll

import scala.annotation.unroll

abstract class Unrolled{
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true): String
}
