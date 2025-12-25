//> using options -Werror -explain

case class A(a: Int)
case class B(b: Int)
case class C(c: Int)

val a = (A(1): A | B) match
  case A(_) => "OK"
  case B(_) => "OK"
  case C(_) => "Not OK" // error // nopos-error for the warning
