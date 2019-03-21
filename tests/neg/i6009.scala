class Foo {
  def foo(f: (erased Int) => Int): Int = ??? // error: Types with erased keyword can only be function types `erased (...) => ...`
}
