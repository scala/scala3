//> using options -experimental
package unroll

import scala.annotation.unroll

case class Unrolled(s: String, n: Int = 1, @unroll b: Boolean = true, @unroll  l: Long = 0){
  def foo = s + n + b + l
}
