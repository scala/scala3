class Foo(val x: Int)

extension (s: Foo)
  def x: Int = 43

@main
def Test =
  val f = Foo(42)
  f.x = 42 // error