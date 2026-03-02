package a

import scala.annotation.unroll

object A {

  def foo(s: String, x: Int, @unroll b: Boolean = true): String = s + x + b

}
