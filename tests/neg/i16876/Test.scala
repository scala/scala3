//> using options -Xfatal-warnings -Wunused:all

object Foo {
  private def myMethod(a: Int, b: Int, c: Int) = adder // ok
  myMethod(1, 2, 3)

  private def myMethodFailing(a: Int, b: Int, c: Int) = a + 0 // warn // warn
  myMethodFailing(1, 2, 3)
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)