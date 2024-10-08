package a
import b.*

import scala.annotation.unroll

// modify A.scala and add a parameter to foo, and add C, third compile via Zinc
object A {

  def foo(s: String, x: Int, @unroll b: Boolean = true): String = s + x + b

}

// C is the same compilation unit as A, and inlines B.caller, so its TASTy will see the forwarder
// where the associated file came from source
object C {
  val res = B.caller
}
