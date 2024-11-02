import caps.use
class Test:
  @use def F = ???  // error
  @use val x = ???  // error
  @use type T       // error
  def foo(@use c: Test): Unit = ??? // OK
