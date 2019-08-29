object Test extends App {
  def foo: String = {
    "abc".asInstanceOf
  }

  assert(foo == "abc")
}
