//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled{
  final def foo[T](s: T, @unroll n: Int = 1, b: Boolean = true, @unroll l: Long = 0) = s.toString + n + b + l
}
