//> using options -Xfatal-warnings -Wunused:all

object Foo {
  private def myMethod(a: Int, b: Int, c: Int) = adder // ok
  myMethod(1, 2, 3)

  private def myMethodFailing(a: Int, b: Int, c: Int) = a + 0 // error // error
  myMethodFailing(1, 2, 3)
}


