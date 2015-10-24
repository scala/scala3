object Test {
  def foo1[T](x: T)(implicit ev: T): Nothing = ???
  def foo2[T](x: T)(implicit ev: T): T = ???

  def test: Unit = {
    implicit val ii: Int = 42

    foo1(10)
    foo2(10)
  }
}

