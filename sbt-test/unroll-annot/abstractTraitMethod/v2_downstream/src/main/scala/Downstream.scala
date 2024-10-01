package unroll

import scala.annotation.unroll

object UnrolledObj extends Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b.toString.take(4)
}

class UnrolledCls extends Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true) = s + n + b.toString.take(4)
}

object UnrollMisc{
  def expectedLength = 6
}