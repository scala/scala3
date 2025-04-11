class Box[T](x: T):
  def m: T = ???
def test1(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val y = Box(io)
    println(y.m)  // warning: another run needs to be scheduled
    val _: () -> Unit = x // error

def test2(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val _: () -> Unit = x // error
    val y = Box(io)
    println(y.m)  // warning: another run needs to be scheduled
    val _: () -> Unit = x // error
