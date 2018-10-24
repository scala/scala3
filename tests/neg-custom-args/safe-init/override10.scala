trait Foo {
  def f: () => String = () => message
  def message: String
  val m = f
}

class Bar extends Foo {
  val message = "hello"
  f()                       // error
  m()                       // error
}