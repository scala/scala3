trait X

sealed trait Y
case object YY extends Y, X
case object ZZ extends Y, X

def foo[A <: X & Y](x: A): Unit =
  x match { // warn
    case YY => ()
  }
