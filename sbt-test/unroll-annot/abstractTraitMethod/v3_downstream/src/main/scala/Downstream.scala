package unroll

import scala.annotation.unroll

object UnrolledObj extends Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true, @unroll l: Long = 0) = s + n + b.toString.take(4) + l
}

class UnrolledCls extends Unrolled {
  def foo(s: String, n: Int = 1, @unroll b: Boolean = true, @unroll l: Long = 0) = s + n + b.toString.take(4) + l
}

object UnrollMisc{
  def expectedLength = 9
}