class Foo1 {
  def foo: implicit String => Int = 1
}

class Foo2 extends Foo1 {
  override def foo: implicit String => Int = 2
}
