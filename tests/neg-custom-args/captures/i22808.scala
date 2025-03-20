class Box[T](x: T):
  def m: T = ???
def test1(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val y = Box(io)
    println(y.m)  // error: another run needs to be scheduled
    val _: () -> Unit = x // was error

def test2(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val _: () -> Unit = x
    val y = Box(io)
    println(y.m)  // error: another run needs to be scheduled
    val _: () -> Unit = x
