package i8847

trait P {
  // The `private` modifier is needed, and `X` must be a case class or case object
  private case class X(x: Int)
  private case object Y
}

trait Foo {
  def foo(): Unit = ???
}

trait Bar extends Foo {
  override def foo(): Unit = super.foo()
}

trait Baz extends Foo {
  override def foo(): Unit = super.foo()
}
