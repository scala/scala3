class Base {
  val x = 10
  @scala.annotation.filled
  def foo: Int = x
}

class Child extends Base {
  private def foo: Int = y  // error

  val y = foo         // error
  val z = super.foo
}