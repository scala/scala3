object Test {
  def foo(x: String, y: x.type): Any = ???
  val f = foo  // error
}
