import caps.{use, Mutable}
class TestUse:
  @use def F = ???  // error
  @use val x = ???  // error
  @use type T       // error
  def foo[@use T](@use c: T): Unit = ??? // error // error

class TestConsume extends Mutable:
  consume def F = ???  // ok
  consume val x = ???  // error
  consume type T       // error

