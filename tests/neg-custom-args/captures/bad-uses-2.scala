import caps.{use, consume}
class TestUse:
  @use def F = ???  // error
  @use val x = ???  // error
  @use type T       // error
  def foo[@use T](@use c: T): Unit = ??? // error // error

class TestConsume:
  @consume def F = ???  // ok
  @consume val x = ???  // error
  @consume type T       // error
  def foo[@consume T](@use c: T): Unit = ??? // error // error

