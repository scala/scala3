//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled{
  final def foo(s: String, @unroll n: Int = 1, b: Boolean = true)(implicit f: String => String) = f(s + n + b)
}
