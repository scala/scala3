object Test extends App {
  class Foo
  class Bar extends Foo

  def overload(implicit foo: Foo): Unit = {}
  def overload(implicit bar: Bar): Unit = {}

  overload(new Bar)
}
