class Foo {
  def foo(x: Any): Unit = {
    x match {
      case y: Bar[?] =>
        y.value match {
          case value: Bar[?] => // here x is an instance of Bar[Bar[_]]
          case _ =>
        }
    }
  }
}

class Bar[T] {
  def value: T = ???
}
