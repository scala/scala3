class Foo {
  def foo(x: Any): Unit = {
    x match {
      case y: Bar[_] =>
        y.value match {
          case value: Bar[_] => // here x is an instance of Bar[Bar[_]]
          case _ =>
        }
    }
  }
}

class Bar[T] {
  def value: T = ???
}
