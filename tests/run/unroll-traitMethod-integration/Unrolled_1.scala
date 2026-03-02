//> using options -experimental
package unroll

trait Unrolled{
  def foo(s: String, n: Int = 1) = s + n
}

object Unrolled extends Unrolled