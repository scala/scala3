//> using options -Xfatal-warnings

trait C[T]
class D[T]

class Test {
  def foo[T](x: C[T]) = x match {
    case _: D[T] =>    // warn
    case _: C[Int] =>  // warn
  }
}
// nopos-error: No warnings can be incurred under -Werror.