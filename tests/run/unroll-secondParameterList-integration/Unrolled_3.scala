//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled{
  final def foo(f: String => String)(s: String, @unroll n: Int = 1, b: Boolean = true, @unroll l: Long = 0) = f(s + n + b + l)
}
