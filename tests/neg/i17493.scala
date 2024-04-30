//> using options -explain -Xfatal-warnings
class A(val s: String) extends AnyVal {
  def g = synchronized { println("hello, world") } // warn
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
