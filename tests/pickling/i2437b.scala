class Foo[T]
object Test {
  def foo[T](arg: Foo[T]) = arg match {
    case bla: Foo[_] =>
  }
}
