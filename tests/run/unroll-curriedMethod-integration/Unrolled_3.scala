//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled{
  final def foo(s: String, @unroll n: Int = 1, b: Boolean = true, @unroll l: Long = 0)(f: String => String) = f(s + n + b + l)
}
