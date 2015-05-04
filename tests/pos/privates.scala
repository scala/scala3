trait Test {

  private val x = 2

  private def foo() = x * x

  private def bar() = foo()

  class Inner {
    foo()
  }

}
