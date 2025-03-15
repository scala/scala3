class Box[T](x: T):
  def m: T = ???
def test1(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val y = Box(io)
    println(y.m)
    val _: () -> Unit = x // error

def test2(io: Object^): Unit =
  def foo(): Unit = bar()
  def bar(): Unit =
    val x = () =>
      foo()
    val _: () -> Unit = x
    val y = Box(io)
    println(y.m)  // error
    val _: () -> Unit = x
