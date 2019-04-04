class Foo {
  def foo[T]: Int = 9

  def bar =
    Option(9) match {
      case Some(foo[Int]) => // error
    }
}
