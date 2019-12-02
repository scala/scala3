object Foo {
  inline def foo = new { bar(0) }

  def bar(i: => Int): Unit = ()
}
