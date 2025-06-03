//> using options -experimental
package unroll

import scala.annotation.unroll

class Unrolled(s: String, n: Int = 1, @unroll b: Boolean = true){
  final def foo = s + n + b
}
