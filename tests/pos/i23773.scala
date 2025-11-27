trait Foo[T]

def foo[A]: Int = ???
def foo[A: Foo]: Int = ???

extension (x: Int)
  def succ: Int = x + 1

val a = foo[Int]
val b = foo[Int].succ // error