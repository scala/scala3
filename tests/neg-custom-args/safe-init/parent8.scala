abstract class Foo {
  val name: String = "Foo"

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `Foo` is a class

  println(title)                        // ok: title is abstract
}

class Qux extends Foo with Bar {
  val x = "hello"
  def title = x                        // error  // error
}
