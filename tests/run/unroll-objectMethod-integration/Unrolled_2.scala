//> using options -experimental
package unroll

import scala.annotation.unroll

object Unrolled{
  final def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b
}
