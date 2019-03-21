class Foo {
  def foo(f: erased (Int) => Int): Int = {
    erased val ctx = 1
    f(ctx)
  }
}
