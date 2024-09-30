package unroll

import scala.annotation.unroll

case class Unrolled(s: String, n: Int = 1, @unroll b: Boolean = true){
  def foo = s + n + b
}
