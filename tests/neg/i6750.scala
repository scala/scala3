object Foo {
  def bar(i: => Int): Unit  = ???
  def bar(l: => Long): Unit = ??? // error
}
