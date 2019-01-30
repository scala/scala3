class Foo1 {
  def foo: given String => Int = 1
}

class Foo2 extends Foo1 {
  override def foo: given String => Int = 2
}
