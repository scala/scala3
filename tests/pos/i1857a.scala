object Test {
  def foo1[T](x: T)(implicit ev: T): Nothing = ???

  def test1: Unit = {
    implicit val ii: Int = 42
    implicit val ss: String = "foo"

    foo1(10) // ambiguous implicit because T=Any
  }
}
