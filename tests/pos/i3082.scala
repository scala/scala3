object Test {
  private def foo(arg1: Int): Int = {
    inline def bar: Int = foo(0)
    if (arg1 == 0) 0 else bar
  }
  assert(foo(11) == 0)
}
