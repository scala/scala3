trait Foo:
  def bar: String

class Baz extends Foo: // error
  val a = "hello"
