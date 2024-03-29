abstract class Foo {
  val name: String = "Foo"

  def title: String
}

trait Bar { this: Foo =>
  val message = "hello, " + name

  println(title)
}

class Qux extends Foo with Bar {
  val x = "hello"       // warn
  def title = x
}
