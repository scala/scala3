abstract class Foo {
  def name: String
  def title: String

  val message = "hello, " + name

  println(title)
}

trait Bar {
  val name: String = "Foo"  // warn

  def title: String = name
}

class Qux extends Foo with Bar {
  val x = "hello"
}
