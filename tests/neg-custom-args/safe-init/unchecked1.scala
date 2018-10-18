abstract class Base {
  def foo: Int
  def bar: Int

  val a = foo + bar
}

class ChildA extends Base {
  val b = 10

  @unchecked
  def foo = b

  def bar = b  // error // error

  val c = this
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