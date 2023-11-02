//> using options -Xfatal-warnings

package Foos

object Outer {
  case class X() // warn
  object x
}

// nopos-error: No warnings can be incurred under -Werror.
