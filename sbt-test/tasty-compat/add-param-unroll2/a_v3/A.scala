package a
import b.*

import scala.annotation.unroll

// modify A.scala and add a parameter to foo, and add C, third compile via Zinc
object A {

  def foo(s: String, x: Int, @unroll b: Boolean = true): String = s + x + b

}

// C is the same compilation unit as A, and inlines B.caller, so its TASTy will attempt to
// resolve the generated forwarder A.foo, which doesn't exist yet in typer phase.
// issue a compilation error here, suggesting to move C to a separate compilation unit.
// In a_v3_2/C.scala, demonstrate fixing the error by moving C to a separate compilation unit.
object C {
  val res: String = B.caller
}
