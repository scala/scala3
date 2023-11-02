//> using options -Xfatal-warnings -feature

package implConv

object B {
  import A.{_, given}

  "".foo

  val x: Int = ""  // ok
  val y: String = 1 // warn: feature
}
// nopos-error: No warnings can be incurred under -Werror.