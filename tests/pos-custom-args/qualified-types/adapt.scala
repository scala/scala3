def f(x: Int): Int = ???
case class IntBox(x: Int)
case class Box[T](x: T)


def f(x: Int, y: Int): {r: Int with r == x + y} = x + y

def test: Unit =
  val x: Int = ???
  def g(x: Int): Int = ???

  val v1: {v: Int with v == x + 1} = x + 1
  val v2: {v: Int with v == f(x)} = f(x)
  val v3: {v: Int with v == g(x)} = g(x)
  //val v6: {v: Int with v == IntBox(x)} = IntBox(x) // Not implemented
  //val v7: {v: Int with v == Box(x)} = Box(x) // Not implemented
  val v4: {v: Int with v == x + f(x)} = x + f(x)
  val v5: {v: Int with v == x + g(x)} = x + g(x)
  val v6: {v: Int with v == f(x + 1)} = f(x + 1)
  val v7: {v: Int with v == g(x + 1)} = g(x + 1)
  //val v12: {v: Int with v == IntBox(x + 1)} = IntBox(x + 1) // Not implemented
  //val v13: {v: Int with v == Box(x + 1)} = Box(x + 1) // Not implemented
