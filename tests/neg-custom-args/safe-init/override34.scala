abstract class Foo {
  def name: String

  val message = "hello, " + name
}

trait Bar {
  val name: String = "Foo"
}

class Qux extends Foo with Bar {  // error
  val x = "hello"
}
