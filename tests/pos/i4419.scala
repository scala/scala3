class Foo(config: String) {
  case class Bar(val x: Int) {
    def doThings: String = config //Do whatever here
  }
}


object Test {
  def test(foo: Foo)(bar: foo.Bar = foo.Bar(5)) = ???

  test(new Foo("port"))()
}
