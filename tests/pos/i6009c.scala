class Foo {
  def foo(f: given erased Int => Int): Int = {
    implicit erased val ctx = 1
    f
  }
}
