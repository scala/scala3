object Test {
  def f(unused foo: Foo) = {
    foo.x() // error
    foo.y // error
    foo.z // error
  }
}

class Foo {
  def x(): String = "abc"
  def y: String = "abc"
  val z: String = "abc"
}
