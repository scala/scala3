abstract class Foo {
  def name: String

  val message = "hello, " + name
}

trait Bar {
  val name: String = "Foo"  // error
}

class Qux extends Foo with Bar {
  val x = "hello"
}
