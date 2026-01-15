//> using options -Werror

case class A(a: Int)
case class B(b: Int)
case class C(c: Int)

val a = (A(1): A) match {
  case A(_) => "OK"
  case B(_) => "NOT OK" // error: this case is unreachable since class A and class B are unrelated
}

val b = (A(1): A | B) match {
  case A(_) => "OK"
  case B(_) => "OK"
  case C(_) => "NOT OK" // error
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
