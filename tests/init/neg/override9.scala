trait Foo {
  def name: String
  val message = "hello, " + name   // error
}

class Bar extends Foo {
  def name = message
}
