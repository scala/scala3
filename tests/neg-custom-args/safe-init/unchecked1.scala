abstract class Base {
  @scala.annotation.partial
  def foo: Int
  @scala.annotation.partial
  def bar: Int

  val a = foo
}

class ChildA extends Base {
  val b = 10

  @unchecked
  def foo = b

  def bar = b  // error // error

  val c = this // error
  @unchecked val d = this // ok
}

@unchecked
class ChildB extends Base {
  val b = 10

  def foo = b
  def bar = b
}


class ChildC extends Base {
  val b = 10

  def foo = b: @unchecked
  def bar = b: @unchecked
}