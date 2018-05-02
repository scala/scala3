abstract class Foo {
  val name: String = "Foo"

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name        // ok: because `Foo` is a class

  println(title)                        // error
}
