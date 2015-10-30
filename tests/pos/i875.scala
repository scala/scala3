class Foo[+A] {
  def foo(a: A => Int = _ => 1): Unit = ()
}
