//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled{
  final def foo[T](s: T, @unroll n: Int = 1, b: Boolean = true) = s.toString + n + b
}
