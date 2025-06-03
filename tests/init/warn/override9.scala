trait Foo {
  def name: String
  val message = "hello, " + name   // warn
}

class Bar extends Foo {
  def name = message
}
