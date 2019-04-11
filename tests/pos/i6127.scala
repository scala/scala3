class Foo[+FT](x: FT) {
  def fooArray: Foo[Array[String]] = new Foo(Array.empty)
  val y: Array[String] = Array.empty
}