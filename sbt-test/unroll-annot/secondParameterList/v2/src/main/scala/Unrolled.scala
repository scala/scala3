package unroll

import scala.annotation.unroll

class Unrolled{
  def foo(f: String => String)(s: String, @unroll n: Int = 1, b: Boolean = true) = f(s + n + b)
}
