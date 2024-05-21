class Foo(_x: Int)

extension (s: Foo)
  def x_=(x: String): Unit = ()
  def x: Int = ???

@main
def Test =
  val f = Foo(42)
  f.x = 42 // error
