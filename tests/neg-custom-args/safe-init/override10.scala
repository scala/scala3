trait Foo {
  def f: () => String = () => message   // error
  def message: String
  val m = f
}

class Bar extends Foo {
  val message = "hello"
  f()
  m()                       // error
}