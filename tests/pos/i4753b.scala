class Foo1 {
  def foo: String |=> Int = 1
}

class Foo2 extends Foo1 {
  override def foo: String |=> Int = 2
}
