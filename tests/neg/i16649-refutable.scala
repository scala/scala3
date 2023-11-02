//> using options -Xfatal-warnings

import quoted.*

def foo(using Quotes)(x: Expr[Int]) =
  val '{ ($y: Int) + ($z: Int) } = x // warn
  val '{ $a: Int } = x
  val '{ $b: Any } = x
  val '{ $c } = x

// nopos-error: No warnings can be incurred under -Werror.
