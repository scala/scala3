//> using options -language:experimental.erasedDefinitions

object Test {
  erased val foo: Foo = new Foo  // error, Foo is not noInits
  foo.x() // error
  foo.y // error
  foo.z // error
}

class Foo {
  def x(): String = "abc"
  def y: String = "abc"
  val z: String = "abc"
}