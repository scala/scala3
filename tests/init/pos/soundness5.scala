class Foo {
  var a: Int = h()
  def h(): Int = g()
  def g(): Int = h()
}