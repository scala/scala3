abstract class Foo {
  def name: String

  val message = "hello, " + name
}

trait Bar {
  val name: String = "Foo"  // warn
}

class Qux extends Foo with Bar {
  val x = "hello"
}
