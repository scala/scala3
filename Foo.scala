object Foo {
  def foo(): Int = macro Bar.fooImpl
}

object Bar {
  def fooImpl(x: Int): Int = ???
}