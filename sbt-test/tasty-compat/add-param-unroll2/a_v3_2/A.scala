package a

import scala.annotation.unroll

// modify A.scala and add a parameter to foo, and add C (in another file), third compile via Zinc
object A {

  def foo(s: String, x: Int, @unroll b: Boolean = true): String = s + x + b

}
