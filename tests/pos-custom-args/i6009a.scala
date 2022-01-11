class Foo {
  def foo(f: (erased Int) => Int): Int = {
    erased val ctx: Int
    f(ctx)
  }
}
